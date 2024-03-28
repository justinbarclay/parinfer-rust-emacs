use crate::types::{Paren, ParenTrail, TabStop};

use super::parinfer::rc_process;
use emacs::{Env, FromLisp, IntoLisp, Result, Value, Vector};
use types::{Change, Error, Options, Request, SharedRequest, WrappedAnswer};

use std::{cell::RefCell, convert::TryFrom, fs::OpenOptions, io::Write, rc::Rc};

emacs::plugin_is_GPL_compatible!();

// This exists because the emacs library doesn't implement
// this type conversion directy
/// A helper function to Option<i64> to Option<usize>
fn to_usize(value: Option<i64>) -> Option<usize> {
  match value {
    Some(item) => match usize::try_from(item) {
      Ok(new_value) => Some(new_value),
      Err(_) => None,
    },
    None => None,
  }
}

fn to_i64(value: Option<usize>) -> Option<i64> {
  match value {
    Some(item) => match i64::try_from(item) {
      Ok(new_value) => Some(new_value),
      Err(_) => None,
    },
    None => None,
  }
}

#[emacs::module(name = "parinfer-rust")]
pub fn init(_: &Env) -> Result<()> {
  Ok(())
}

////////////////////////////////
// Entry point
///////////////////////////////
// Need to wrap Request into a specific lifetime for use as part of this struct are used in the Answer struct
// this is talked about here https://github.com/ubolonton/emacs-module-rs/issues/21
type AliasedRequest<'a> = &'a SharedRequest;

//  https://github.com/shaunlebron/parinfer/tree/master/lib#api
// text is the full text input.
// options is an object with the following properties:
//   cursorLine - zero-based line number of the cursor
//   cursorX - zero-based x-position of the cursor
//   prevCursorLine and prevCursorX is required by Smart Mode (previous cursor position)
//   selectionStartLine - first line of the current selection
//   changes - ordered array of change objects with the following:
//     lineNo - starting line number of the change
//     x - starting x of the change
//     oldText - original text that was replaced
//     newText - new text that replaced the original text
//   forceBalance - employ the aggressive paren-balancing rules from v1 (defaults to false)
//   partialResult - return partially processed text/cursor if an error occurs (defaults to false)

// success is a boolean indicating if the input was properly formatted enough to create a valid result
// text is the full text output (if success is false, returns original text unless partialResult is enabled)
// cursorX/cursorLine is the new position of the cursor (since parinfer may shift it around)
// error is an object populated if success is false:
//   name is the name of the error, which will be any of the following:
//     "quote-danger"
//     "eol-backslash"
//     "unclosed-quote"
//     "unclosed-paren"
//     "unmatched-close-paren"
//     "unhand led"
//   message is a message describing the error
//   lineNo is a zero-based line number where the error occurred
//   x is a zero-based column where the error occurred
//   extra has lineNo and x of open-paren for unmatched-close-paren
// tabStops is an array of objects representing Tab stops, which is populated if a cursor position or selection is supplied. We identify tab stops at relevant open-parens, and supply the following extra information so you may compute extra tab stops for one-space or two-space indentation conventions based on the type of open-paren.
//   x is a zero-based x-position of the tab stop
//   argX position of the first argument after x (e.g. position of bar in (foo bar)
//   lineNo is a zero-based line number of the open-paren responsible for the tab stop
//   ch is the character of the open-paren responsible for the tab stop (e.g. (,[,{)
// parenTrails is an array of object representing the Paren Trails at the end of each line that Parinfer may move
//   lineNo is a zero-based line number
//   startX is a zero-based x-position of the first close-paren
//   endX is a zero-based x-position after the last close-paren
#[defun(user_ptr, mod_in_name = false)]
/// Runs the parinfer algorithm on the given request
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-execute request)
/// ```
fn execute(request: AliasedRequest) -> Result<WrappedAnswer> {
  let answer = rc_process(&request);
  let wrapped_answer = unsafe { WrappedAnswer::new(request.clone(), answer) };
  Ok(wrapped_answer)
}
////////////////////////////////
// options
////////////////////////////////
#[defun(user_ptr, mod_in_name = false)]
// Create an Options Structure
// We need this because we can't pass in an optional variant of Options in the new_options function
/// Returns an Option with nil/default data for all fields
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-make-option)
/// ```
fn make_option() -> Result<Options> {
  Ok(Default::default())
}

#[defun(user_ptr, mod_in_name = false)]
/// Creates an Options type based on inputs
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-new-option 1 1 nil options changes)
/// ```
fn new_options(
  cursor_x: Option<i64>,
  cursor_line: Option<i64>,
  selection_start_line: Option<i64>,
  old_options: &Options,
  changes: &Vec<Change>,
) -> Result<Options> {
  Ok(Options {
    cursor_x: to_usize(cursor_x),
    cursor_line: to_usize(cursor_line),
    prev_cursor_x: old_options.cursor_x,
    prev_cursor_line: old_options.cursor_line,
    selection_start_line: to_usize(selection_start_line),
    changes: changes.clone(),
    prev_text: None,
    ..old_options.clone()
  })
}

emacs::define_errors! {
    unknown_option_error "This option name is unknown" (error)
}

#[defun(user_ptr, mod_in_name = false)]
/// Set a field within the passed options.
///
/// Valid field names are:
/// - `:partial-result'
/// - `:force-balance'
/// - `:return-parens'
/// - `:comment-char'
/// - `:string-delimiters'
/// - `:lisp-vline-symbols'
/// - `:lisp-block-comments'
/// - `:guile-block-comments'
/// - `:scheme-sexp-comments'
/// - `:janet-long-strings'
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-set-option options 'partial-result t)
/// ```
fn set_option<'a>(
  options: &mut Options,
  option_name: Value<'a>,
  new_value: Option<Value<'a>>,
) -> Result<()> {
  let env = option_name.env;
  if option_name.eq(env.intern(":partial-result")?) {
    options.partial_result = new_value
      .map(|val| val.is_not_nil())
      .unwrap_or_else(Options::default_false);
    return Ok(());
  }
  if option_name.eq(env.intern(":force-balance")?) {
    options.force_balance = new_value
      .map(|val| val.is_not_nil())
      .unwrap_or_else(Options::default_false);
    return Ok(());
  }
  if option_name.eq(env.intern(":return-parens")?) {
    options.return_parens = new_value
      .map(|val| val.is_not_nil())
      .unwrap_or_else(Options::default_false);
    return Ok(());
  }
  if option_name.eq(env.intern(":comment-char")?) {
    options.comment_char = new_value
      .map(|val| String::from_lisp(val))
      .transpose()?
      .map(|char_as_str| char_as_str.chars().next())
      .flatten()
      .unwrap_or_else(Options::default_comment);
    return Ok(());
  }
  if option_name.eq(env.intern(":string-delimiters")?) {
    if let Some(new_value) = new_value {
      let vector = Vector::from_lisp(new_value)?;
      let rust_values = vector
        .into_iter()
        .map(|inner_value| String::from_lisp(inner_value))
        .collect::<Result<Vec<String>>>()?;
      options.string_delimiters = rust_values;
    } else {
      options.string_delimiters = Options::default_string_delimiters();
    }
    return Ok(());
  }
  if option_name.eq(env.intern(":lisp-vline-symbols")?) {
    options.lisp_vline_symbols = new_value
      .map(|val| val.is_not_nil())
      .unwrap_or_else(Options::default_false);
    return Ok(());
  }
  if option_name.eq(env.intern(":lisp-block-comments")?) {
    options.lisp_block_comments = new_value
      .map(|val| val.is_not_nil())
      .unwrap_or_else(Options::default_false);
    return Ok(());
  }
  if option_name.eq(env.intern(":guile-block-comments")?) {
    options.guile_block_comments = new_value
      .map(|val| val.is_not_nil())
      .unwrap_or_else(Options::default_false);
    return Ok(());
  }
  if option_name.eq(env.intern(":scheme-sexp-comments")?) {
    options.scheme_sexp_comments = new_value
      .map(|val| val.is_not_nil())
      .unwrap_or_else(Options::default_false);
    return Ok(());
  }
  if option_name.eq(env.intern(":janet-long-strings")?) {
    options.janet_long_strings = new_value
      .map(|val| val.is_not_nil())
      .unwrap_or_else(Options::default_false);
    return Ok(());
  }

  env.signal(unknown_option_error, [option_name])
}

#[defun(mod_in_name = false)]
/// Get a field within the passed options.
///
/// Valid field names are:
/// - `partial-result'
/// - `force-balance'
/// - `return-parens'
/// - `comment-char'
/// - `string-delimiters'
/// - `lisp-vline-symbols'
/// - `lisp-block-comments'
/// - `guile-block-comments'
/// - `scheme-sexp-comments'
/// - `janet-long-strings'
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-get-option options 'partial-result)
/// ```
fn get_option<'a>(options: &Options, option_name: Value<'a>) -> Result<Value<'a>> {
  // The function is returning a type-erased Value because it can either be a boolean
  // or a list
  let env = option_name.env;
  if option_name.eq(env.intern(":partial-result")?) {
    return Ok(options.partial_result.into_lisp(env)?);
  }
  if option_name.eq(env.intern(":force-balance")?) {
    return Ok(options.force_balance.into_lisp(env)?);
  }
  if option_name.eq(env.intern(":return-parens")?) {
    return Ok(options.return_parens.into_lisp(env)?);
  }
  if option_name.eq(env.intern(":comment-char")?) {
    return Ok(options.comment_char.to_string().into_lisp(env)?);
  }
  if option_name.eq(env.intern(":string-delimiters")?) {
    // return Ok(to_lisp_vec(env, options.string_delimiters.clone())?);
    return Ok(VecToVector(options.string_delimiters.clone()).into_lisp(env)?);
  }
  if option_name.eq(env.intern(":lisp-vline-symbols")?) {
    return Ok(options.lisp_vline_symbols.into_lisp(env)?);
  }
  if option_name.eq(env.intern(":lisp-block-comments")?) {
    return Ok(options.lisp_block_comments.into_lisp(env)?);
  }
  if option_name.eq(env.intern(":guile-block-comments")?) {
    return Ok(options.guile_block_comments.into_lisp(env)?);
  }
  if option_name.eq(env.intern(":scheme-sexp-comments")?) {
    return Ok(options.scheme_sexp_comments.into_lisp(env)?);
  }
  if option_name.eq(env.intern(":janet-long-strings")?) {
    return Ok(options.janet_long_strings.into_lisp(env)?);
  }

  env.signal(unknown_option_error, [option_name])
}

// Make a wrapper type to convince the compiler that the
// lifetimes of the associated lisp environment are going to be
// fine
struct VecToVector(Vec<String>);

impl<'e> IntoLisp<'e> for VecToVector {
  fn into_lisp(self, env: &'e Env) -> Result<Value<'e>> {
    env.vector(
      self
        .0
        .into_iter()
        .map(|s| s.into_lisp(env))
        .collect::<Result<Vec<Value>>>()?
        .as_slice(),
    )
  }
}

#[defun(mod_in_name = false)]
/// Returns a string representation of the Options type
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-print-option options)
/// ```
fn print_options<'a>(options: &Options) -> Result<String> {
  Ok(format!("{:?}", options.clone()).to_string())
}

////////////////////////////////
// Changes
////////////////////////////////
#[defun(user_ptr, mod_in_name = false)]

/// Creates an empty list of changes
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-make-changes)
/// ```
fn make_changes() -> Result<Vec<Change>> {
  Ok(Vec::new())
}
#[defun(user_ptr, mod_in_name = false)]
fn new_change(line_number: i64, x: i64, old_text: String, new_text: String) -> Result<Change> {
  let line_no = usize::try_from(line_number)?;
  let new_x = usize::try_from(x)?;
  let change = Change {
    x: new_x,
    line_no,
    old_text,
    new_text,
  };
  Ok(change)
}

#[defun(mod_in_name = false)]
/// Creates an empty list of changes
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-make-changes)
/// ```
fn add_change(changes: &mut Vec<Change>, change: &Change) -> Result<()> {
  Ok(changes.push(change.clone()))
}

#[defun(mod_in_name = false)]
/// Returns a string representing a list of changes
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-print-changes changes)
/// ```
fn print_changes<'a>(env: &'a Env, changes: &mut Vec<Change>) -> Result<Value<'a>> {
  format!("{:?}", changes).into_lisp(env)
}

////////////////////////////////
// Request
////////////////////////////////
#[defun(mod_in_name = false)]

/// Creates a Request from the given mode, current buffer text, and the set of Options
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-make-request "paren" (buffer-substring-no-properties) options)
/// ```
//
fn make_request(mode: String, text: String, options: &mut Options) -> Result<SharedRequest> {
  let request = Request {
    mode,
    text,
    options: options.clone(),
  };
  Ok(Rc::new(request))
}

/// Creates a Request from the given mode, current buffer text, and the set of Options
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer--rust-print-request request)
/// ```
//
#[defun(mod_in_name = false)]
fn print_request(request: AliasedRequest) -> Result<String> {
  Ok(format!("{:?}", &request).to_string())
}
////////////////////////////////
// Answer
////////////////////////////////

enum AnswerKey {
  Text,
  Success,
  Error,
  CursorX,
  CursorLine,
  TabStops,
  ParenTrails,
  Parens,
}

impl FromLisp<'_> for AnswerKey {
  fn from_lisp(value: Value<'_>) -> Result<Self> {
    let env = value.env;

    if value.eq(env.intern(":text")?) {
      Ok(AnswerKey::Text)
    } else if value.eq(env.intern(":success")?) {
      Ok(AnswerKey::Success)
    } else if value.eq(env.intern(":error")?) {
      Ok(AnswerKey::Error)
    } else if value.eq(env.intern(":cursor-x")?) {
      Ok(AnswerKey::CursorX)
    } else if value.eq(env.intern(":cursor-line")?) {
      Ok(AnswerKey::CursorLine)
    } else if value.eq(env.intern(":tab-stops")?) {
      Ok(AnswerKey::TabStops)
    } else if value.eq(env.intern(":paren-trails")?) {
      Ok(AnswerKey::ParenTrails)
    } else if value.eq(env.intern(":parens")?) {
      Ok(AnswerKey::Parens)
    } else {
      env.signal(unknown_option_error, [value])
    }
  }
}

impl IntoLisp<'_> for TabStop<'_> {
  fn into_lisp(self, env: &Env) -> Result<Value> {
    env.list(&[
      env.intern(":x")?,
      self.x.into_lisp(env)?,
      env.intern(":arg-x")?,
      self.arg_x.into_lisp(env)?,
      env.intern(":line-no")?,
      self.line_no.into_lisp(env)?,
      env.intern(":ch")?,
      self.ch.into_lisp(env)?,
    ])
  }
}

impl IntoLisp<'_> for ParenTrail {
  fn into_lisp(self, env: &Env) -> Result<Value> {
    env.list(&[
      env.intern(":line-no")?,
      self.line_no.into_lisp(env)?,
      env.intern(":start-x")?,
      self.start_x.into_lisp(env)?,
      env.intern(":end-x")?,
      self.end_x.into_lisp(env)?,
    ])
  }
}

struct ParenTrails(Vec<ParenTrail>);

impl IntoLisp<'_> for ParenTrails {
  fn into_lisp(self, env: &Env) -> Result<Value> {
    env.list(
      self
        .0
        .into_iter()
        .map(|paren_trail| paren_trail.into_lisp(env))
        .collect::<Result<Vec<Value>>>()?
        .as_slice(),
    )
  }
}

impl IntoLisp<'_> for Paren<'_> {
  fn into_lisp(self, env: &Env) -> Result<Value> {
    env.list(&[
      env.intern(":line-no")?,
      self.line_no.into_lisp(env)?,
      env.intern(":x")?,
      self.x.into_lisp(env)?,
    ])
  }
}

struct Parens<'a>(Vec<Paren<'a>>);

impl IntoLisp<'_> for Parens<'_> {
  fn into_lisp(self, env: &Env) -> Result<Value> {
    env.list(
      self
        .0
        .into_iter()
        .map(|paren| paren.into_lisp(env))
        .collect::<Result<Vec<Value>>>()?
        .as_slice(),
    )
  }
}

struct TabStops<'a>(Vec<TabStop<'a>>);

impl IntoLisp<'_> for TabStops<'_> {
  fn into_lisp(self, env: &Env) -> Result<Value> {
    env.list(
      self
        .0
        .into_iter()
        .map(|tab_stop| tab_stop.into_lisp(env))
        .collect::<Result<Vec<Value>>>()?
        .as_slice(),
    )
  }
}

impl IntoLisp<'_> for Error {
  fn into_lisp(self, env: &Env) -> Result<Value> {
    env.list(&[
      env.intern(":name")?,
      self.name.to_string().into_lisp(env)?,
      env.intern(":message")?,
      self.message.into_lisp(env)?,
      env.intern(":line_no")?,
      self.line_no.into_lisp(env)?,
      env.intern(":x")?,
      self.x.into_lisp(env)?,
    ])
  }
}

#[defun(mod_in_name = false)]
/// Gives a hashmap like interface to extracting values from the Answer type
/// Accepted keys are 'text', 'success', 'cursor_x', 'cursor_line', and 'error'
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-get-answer answer "success")
/// ```
fn get_answer<'a>(env: &'a Env, answer: &WrappedAnswer, key: AnswerKey) -> Result<Value<'a>> {
  let unwrapped_answer = answer.inner();
  match key {
    AnswerKey::Text => unwrapped_answer.text.to_string().into_lisp(env),
    AnswerKey::Success => unwrapped_answer.success.into_lisp(env),
    AnswerKey::Error => match unwrapped_answer.error.clone() {
      Some(error) => Ok(RefCell::new(error).into_lisp(env)?),
      None => ().into_lisp(env),
    },
    AnswerKey::CursorX => to_i64(unwrapped_answer.cursor_x).into_lisp(env),
    AnswerKey::CursorLine => to_i64(unwrapped_answer.cursor_line).into_lisp(env),
    // I only care about some nested structures at the moment, errors,
    // so leave tab_stops, paren_trails, and parens as unsupported
    AnswerKey::TabStops => TabStops(unwrapped_answer.tab_stops.clone()).into_lisp(env),
    AnswerKey::ParenTrails => {
      env.message("Paren trails are not supported")?;
      ().into_lisp(env)
    }
    AnswerKey::Parens => {
      env.message("Parens are not supported")?;
      ().into_lisp(env)
    }
  }
}

#[defun(mod_in_name = false)]
/// Returns a string representation of an Answer
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-print-answer answer)
/// ```
fn print_answer(answer: &WrappedAnswer) -> Result<String> {
  Ok(format!("{:?}", answer.inner()).to_string())
}

#[defun(mod_in_name = false)]
/// Prints the current Options and Answer to the specified file
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-debug "/tmp/parinfer.txt" options answer)
/// ```
fn debug(
  env: &Env,
  filename: String,
  options: &Options,
  wrapped_answer: &WrappedAnswer,
) -> Result<()> {
  let answer = wrapped_answer.inner();
  let file = match OpenOptions::new().append(true).create(true).open(&filename) {
    Ok(file) => file,
    Err(_) => {
      env.message(&format!("Unable to open file {}", filename))?;
      return Ok(());
    }
  };

  match write!(&file, "Options:\n{:?}\nResponse:\n{:?}\n", options, answer) {
    Ok(_) => {
      env.message(&format!("Wrote debug information to {}", filename))?;
    }
    Err(_) => {
      env.message(&format!("Unable to write to file {}", filename))?;
    }
  };
  Ok(())
}

////////////////////////////////
// Error
////////////////////////////////
#[defun(mod_in_name = false)]
/// Returns a string representation of an Error
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-print-error error)
/// ```
fn print_error(error: &Error) -> Result<String> {
  Ok(format!("{:?}", error).to_string())
}

#[defun(mod_in_name = false)]
/// Returns the version of the parinfer-rust library
///
/// # Examples
///
/// ```elisp,no_run
/// (parinfer-rust-version)
/// ```
fn version() -> Result<String> {
  Ok(env!("CARGO_PKG_VERSION").to_string())
}
