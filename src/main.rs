//! A simplified grep implementation in Rust.
//!
//! This module implements a basic regex engine that supports a subset of regular expression
//! syntax, including:
//! - Literal character matching
//! - Character classes (`[abc]`, `[a-z]`, `[^abc]`)
//! - Escape sequences (`\d`, `\s`, `\w`)
//! - Anchors (`^` for start, `$` for end)
//! - Quantifiers (`+` for one-or-more)
//!
//! The implementation follows a classic tokeniser → parser → matcher architecture.

#![warn(clippy::all)]
use std::env;
use std::io;
use std::process;

#[derive(thiserror::Error, Debug)]
enum GrepError {
    #[error("Could not parse pattern")]
    InvalidPattern,

    #[error("IO error")]
    IoError(#[from] io::Error),
}

struct Regex {
    tokens: Vec<Token>,
    exprs: Vec<Expr>,
}

impl Regex {
    pub fn new(pattern: &str) -> Result<Self, GrepError> {
        let tokens = tokenise(pattern)?;
        let exprs = parse(&tokens)?;
        Ok(Regex { tokens, exprs })
    }

    fn matches(&self, input: &str) -> Result<bool, GrepError> {
        match_pattern(input, &self.tokens, &self.exprs)
    }
}

/// Represents a single token in the regex pattern.
///
/// Tokens are the output of lexical analysis (tokenisation) and represent
/// the atomic elements of a regex pattern before parsing into expressions.
#[derive(Debug, Clone)]
enum Token {
    /// Matches any ASCII digit (0-9). Corresponds to `\d`.
    Digit,
    /// Matches any whitespace character. Corresponds to `\s`.
    Whitespace,
    /// Matches a character class, e.g., `[a-z]` or `[^abc]`.
    Range(String),
    /// Matches a literal string exactly.
    Literal(String),
    /// Matches any word character (alphanumeric). Corresponds to `\w`.
    WordCharacter,
    /// Anchors the match to the start of the input. Corresponds to `^`.
    StartAnchor,
    /// Anchors the match to the end of the input. Corresponds to `$`.
    EndAnchor,
    /// Quantifier: matches the preceding token one or more times. Corresponds to `+`.
    OneOrMore,
}

/// Represents a parsed expression node in the regex AST.
///
/// Expressions form the abstract syntax tree (AST) after parsing tokens.
/// Unlike tokens, expressions can be nested (e.g., `OneOrMore` wraps another `Expr`).
#[derive(Debug, Clone)]
enum Expr {
    /// Matches any ASCII digit (0-9).
    Digit,
    /// Matches any whitespace character.
    Whitespace,
    /// Matches any word character (alphanumeric or underscore).
    WordCharacter,
    /// Matches a character class or range.
    Range(String),
    /// Matches a literal string exactly.
    Literal(String),
    /// Asserts position is at the start of the input (zero-width).
    StartAnchor,
    /// Asserts position is at the end of the input (zero-width).
    EndAnchor,
    /// Matches the inner expression one or more times (greedy).
    OneOrMore(Box<Expr>),
}

/// Tokenises a simplified regular expression pattern string into a vector of `Token`s.
///
/// # Arguments
///
/// * `input` - A string slice representing the regex pattern to tokenise.
///
/// # Returns
///
/// * `Vec<Token>` - A vector of tokens representing the parsed elements of the pattern.
///
/// # Supported Tokens
///
/// - `Token::Digit`: Represents the `\d` escape sequence for digits.
/// - `Token::Whitespace`: Represents the `\s` escape sequence for whitespace.
/// - `Token::WordCharacter`: Represents the `\w` escape sequence for word characters.
/// - `Token::Range(String)`: Represents a character class or range, e.g., `[0-9]`, `[a-z]`.
/// - `Token::Literal(String)`: Represents literal substrings or characters.
/// - `Token::StartAnchor`: Represents `^` at the start of the pattern.
/// - `Token::EndAnchor`: Represents `$` at the end of the pattern.
/// - `Token::OneOrMore`: Represents `+`.
///
/// # Behavior
///
/// - Recognizes escape sequences like `\d` and `\s`.
/// - Parses character classes enclosed in brackets, e.g., `[abc]`, `[0-9]`.
/// - Treats unmatched `[` as a literal.
/// - Ignores whitespace outside of character classes.
/// - Groups consecutive literal characters into a single `Token::Literal`.
///
/// # Example
///
/// ```
/// let tokens = tokenise(r"\d [a-z] cat");
/// // tokens: [Token::Digit, Token::Range("[a-z]".to_string()), Token::Literal("cat".to_string())]
/// ```
///
fn tokenise(input: &str) -> Result<Vec<Token>, GrepError> {
    let mut tokens = Vec::new();
    let mut chars = input.char_indices().peekable();

    while let Some((i, chr)) = chars.next() {
        match chr {
            '^' => {
                // ^ is only a start anchor when it appears at the beginning of the pattern
                // This follows standard regex semantics where ^ anywhere else is literal
                if i == 0 {
                    tokens.push(Token::StartAnchor);
                } else {
                    tokens.push(Token::Literal("^".to_string()));
                }
            }
            '\\' => {
                match_escaped(&mut chars, &mut tokens);
            }
            '[' => {
                // Parse a character class like [0-9]
                // Look for the closing bracket
                let remainder = &input[i..];
                if let Some(end_pos) = remainder.find(']') {
                    let range = &remainder[..=end_pos];
                    tokens.push(Token::Range(range.to_string()));
                    // Skip ahead, past the range
                    for _ in 0..end_pos {
                        chars.next();
                    }
                } else {
                    // No closing bracket, treat as a literal
                    tokens.push(Token::Literal("[".to_string()));
                }
            }
            '$' => {
                // $ is only an end anchor when it appears at the end of the pattern
                // This follows standard regex semantics where $ anywhere else is literal
                if chars.peek().is_none() {
                    tokens.push(Token::EndAnchor);
                } else {
                    tokens.push(Token::Literal("$".to_string()));
                }
            }
            '+' => {
                // One or more unless escaped. Only makes sense if it follows another token
                if tokens.is_empty() {
                    return Err(GrepError::InvalidPattern);
                }
                tokens.push(Token::OneOrMore);
            }
            _ => {
                // Collect consecutive literal characters
                let start_pos = i;
                let mut end_pos = i + chr.len_utf8();

                while let Some((pos, chr)) = chars.peek() {
                    // If we hit one of these chars, we need to break out so we can handle them in the
                    // outer loop, as they could indicate a special character.
                    if matches!(chr, '\\' | '[' | '$' | '+' | '^') {
                        break;
                    }
                    end_pos = pos + chr.len_utf8();
                    chars.next();
                }
                // If + follows, the last character in our batch is its target —
                // split it off into its own token so the parser can wrap it correctly.
                if matches!(chars.peek(), Some((_, '+'))) {
                    let split_pos = input[start_pos..end_pos]
                        .char_indices()
                        .last()
                        .map_or(start_pos, |(i, _)| start_pos + i);
                    if split_pos > start_pos {
                        tokens.push(Token::Literal(input[start_pos..split_pos].to_string()));
                    }
                    tokens.push(Token::Literal(input[split_pos..end_pos].to_string()));
                } else {
                    tokens.push(Token::Literal(input[start_pos..end_pos].to_string()));
                }
            }
        }
    }
    Ok(tokens)
}

fn match_escaped(chars: &mut std::iter::Peekable<std::str::CharIndices>, tokens: &mut Vec<Token>) {
    if let Some((_, next_chr)) = chars.peek() {
        match next_chr {
            'd' => {
                tokens.push(Token::Digit);
                chars.next();
            }
            's' => {
                tokens.push(Token::Whitespace);
                chars.next();
            }
            'w' => {
                tokens.push(Token::WordCharacter);
                chars.next();
            }
            '+' => {
                tokens.push(Token::Literal("+".to_string()));
                chars.next();
            }
            '^' => {
                tokens.push(Token::Literal("^".to_string()));
                chars.next();
            }
            '$' => {
                tokens.push(Token::Literal("$".to_string()));
                chars.next();
            }
            '[' => {
                tokens.push(Token::Literal("[".to_string()));
                chars.next();
            }
            ']' => {
                tokens.push(Token::Literal("]".to_string()));
                chars.next();
            }
            '\\' => {
                // Handle literal backslack - '\\' becomes a literal '\'
                tokens.push(Token::Literal(r"\".into()));
                chars.next();
            }
            other => {
                eprintln!("Unknown escape sequence: {other}");
                chars.next();
            }
        }
    } else {
        // Trailing backslash, so treat as a literal char
        tokens.push(Token::Literal("\\".to_string()));
    }
}

/// Parses a slice of tokens into an abstract syntax tree (AST) expression.
///
/// This function takes a sequence of tokenized regex pattern elements and converts
/// them into a hierarchical expression structure that can be evaluated for pattern
/// matching. Each token type is mapped to its corresponding expression type.
///
/// # Arguments
///
/// * `tokens` - A slice of `Token` enum variants representing the tokenized regex pattern
///
/// # Returns
///
/// Returns `Vec<Expr>`, where each expression corresponds to a token from the input.
/// The `Vec` represents the entire parsed regex pattern.
///
/// # Token to Expression Mapping
///
/// * `Token::Digit` → `Expr::Digit` - Matches any digit character (0-9)
/// * `Token::Whitespace` → `Expr::Whitespace` - Matches whitespace characters
/// * `Token::Range(r)` → `Expr::Range(r)` - Matches character ranges like [a-z]
/// * `Token::Literal(l)` → `Expr::Literal(l)` - Matches literal string characters
/// * `Token::WordCharacter` → `Expr::WordCharacter` - Matches word characters (\w)
/// * `Token::StartAnchor` → `Expr::StartAnchor` - Asserts position at start of input
/// * `Token::EndAnchor` → `Expr::EndAnchor` - Asserts position at end of input
/// * `Token::OneOrMore` → `Expr::OneOrMore(prev)` - Wraps the preceding expression for 1+ matching
///
/// # Example
///
/// ```
/// let tokens = vec![Token::Literal("hello".to_string()), Token::Digit];
/// let exprs = parse(&tokens).unwrap();
/// // exprs: vec![Expr::Literal("hello".to_string()), Expr::Digit]
/// ```
fn parse(tokens: &[Token]) -> Result<Vec<Expr>, GrepError> {
    let mut exprs = vec![];

    for token in tokens {
        let expr = match token {
            Token::Digit => Expr::Digit,
            Token::Whitespace => Expr::Whitespace,
            Token::Range(r) => Expr::Range(r.to_owned()),
            Token::Literal(l) => Expr::Literal(l.to_owned()),
            Token::WordCharacter => Expr::WordCharacter,
            Token::StartAnchor => Expr::StartAnchor,
            Token::EndAnchor => Expr::EndAnchor,
            Token::OneOrMore => match exprs.pop() {
                Some(expr) => Expr::OneOrMore(Box::new(expr)),
                None => return Err(GrepError::InvalidPattern),
            },
        };
        exprs.push(expr);
    }
    Ok(exprs)
}

/// Attempts to match the first expression in the slice at a specific position.
///
/// This function evaluates a single expression against the input string starting
/// at `start_pos`. It returns the new position after the match if successful,
/// or `None` if the match fails.
///
/// # Arguments
///
/// * `input` - The input string to match against
/// * `exprs` - A slice of expressions; only the first expression is evaluated
/// * `start_pos` - The character position to start matching from
///
/// # Returns
///
/// * `Some(new_pos)` - The position after the matched content (may be same as `start_pos` for anchors)
/// * `None` - If the expression does not match at this position
///
fn match_exprs(input: &[char], exprs: &[Expr], start_pos: usize) -> Option<usize> {
    if let Some(expr) = exprs.first() {
        let rest = &exprs[1..];
        match expr {
            Expr::Digit => {
                let char = get_char_at(input, start_pos)?;
                if char.is_ascii_digit() {
                    match_exprs(input, rest, start_pos + 1)
                } else {
                    None
                }
            }
            Expr::Whitespace => {
                let char = get_char_at(input, start_pos)?;
                if char.is_whitespace() {
                    match_exprs(input, rest, start_pos + 1)
                } else {
                    None
                }
            }
            Expr::Range(range_str) => {
                let char = get_char_at(input, start_pos)?;
                if matches_range(char, range_str) {
                    match_exprs(input, rest, start_pos + 1)
                } else {
                    None
                }
            }
            Expr::Literal(literal_str) => {
                let literal_chars: Vec<char> = literal_str.chars().collect();
                if input[start_pos..].starts_with(&literal_chars) {
                    // Need to make sure we move past the matched literal, hence the + literal_chars.len()
                    match_exprs(input, rest, start_pos + literal_chars.len())
                } else {
                    None
                }
            }
            Expr::WordCharacter => {
                let char = get_char_at(input, start_pos)?;
                if char.is_ascii_alphanumeric() || char == '_' {
                    match_exprs(input, rest, start_pos + 1)
                } else {
                    None
                }
            }
            Expr::StartAnchor => {
                // ^ Asserts we're at the beginning; doesn't consume any characters
                // Returns the same position if we're at the start, None otherwise
                if start_pos == 0 {
                    match_exprs(input, rest, start_pos)
                } else {
                    None
                }
            }
            Expr::EndAnchor => {
                // $ asserts we're at the end - it doesn't consume characters
                // start_pos == input.len() means we're just past the last character
                if start_pos == input.len() {
                    Some(start_pos)
                } else {
                    None
                }
            }
            Expr::OneOrMore(prev_expr) => {
                // The `+` quantifier greedily matches the previous character
                let mut pos = start_pos;
                let mut positions = Vec::new();

                while let Some(new_pos) = match_exprs(input, &[*prev_expr.clone()], pos) {
                    if new_pos == pos {
                        break;
                    } // zero-width guard
                    pos = new_pos;
                    positions.push(pos);
                }
                // Backtrack phase: try from greediest to least greedy
                for &candidate in positions.iter().rev() {
                    if let Some(end) = match_exprs(input, rest, candidate) {
                        return Some(end);
                    }
                }
                None
            }
        }
    } else {
        Some(start_pos)
    }
}

/// Determines if a character matches a regex character class (range).
///
/// Handles various character class formats following regex semantics:
/// - Simple lists: [abc] matches 'a', 'b', or 'c'
/// - Ranges: [a-z] matches any lowercase letter
/// - Negated classes: [^abc] matches anything except 'a', 'b', or 'c'
///
/// # Arguments
///
/// * `chr` - The character to test
/// * `range_str` - The character class string including brackets, e.g., "[a-z]"
///
/// # Returns
///
/// * `bool` - true if the character matches the class, false otherwise
fn matches_range(chr: char, range_str: &str) -> bool {
    // Remove the brackets
    let inner = &range_str[1..range_str.len() - 1];

    // Handle negated character classes: [^abc] means "not a, b, or c"
    // The ^ inside brackets has different meaning than ^ at pattern start
    if inner.starts_with('^') {
        return !inner.contains(chr);
    }

    // Handle simple character lists like [abc] - just check if char is in the list
    // Only do this if there's no dash to avoid conflicting with range syntax
    if !inner.contains('-') {
        return inner.contains(chr);
    }

    // Handle character ranges like [a-z] or [0-9]
    // We only support simple ranges with single characters on each side
    let parts: Vec<&str> = inner.split('-').collect();
    if parts.len() == 2 && parts[0].len() == 1 && parts[1].len() == 1 {
        let start = parts[0].chars().next().unwrap();
        let end = parts[1].chars().next().unwrap();
        return chr >= start && chr <= end;
    }

    // Fallback for complex patterns: treat as character list
    // This handles edge cases like [a-] or [-z] where dash is literal
    inner.contains(chr)
}

/// Safely retrieves a character at a given position in a string.
///
/// Returns None if the position is beyond the string bounds, avoiding panics.
/// This is essential for safe pattern matching where we might probe past string end.
///
/// # Arguments
///
/// * `input` - The string to index into
/// * `pos` - The character position (not byte position)
///
/// # Returns
///
/// * `Option<char>` - The character at that position, or None if out of bounds
fn get_char_at(input: &[char], pos: usize) -> Option<char> {
    input.get(pos).copied()
}

/// Determines if a regex pattern matches anywhere in the input string.
///
/// This function implements the main matching strategy, handling anchors specially:
/// - Start anchors (^) constrain matching to only begin at position 0
/// - End anchors ($) are handled by the `match_expr` logic, ensuring matches end at string end
/// - Without anchors, we try matching at every position
///
/// # Arguments
///
/// * `input_line` - The string to search within
/// * `pattern` - The regex pattern to match
///
/// # Returns
///
/// * `bool` - true if the pattern matches anywhere in the input, false otherwise
fn match_pattern(input_line: &str, tokens: &[Token], exprs: &[Expr]) -> Result<bool, GrepError> {
    // Check if the pattern starts with a start anchor
    let has_start_anchor = matches!(tokens.first(), Some(Token::StartAnchor));
    let input_chars: Vec<char> = input_line.chars().collect();

    if has_start_anchor {
        // Start anchor means the pattern must match from the very beginning
        // No need to try other positions - this is how ^ works in regex
        return Ok(match_exprs(&input_chars, &exprs, 0).is_some());
    }
    // Without start anchor, try matching at every position in the input
    // This implements the standard regex behavior of finding matches anywhere
    for i in 0..input_line.len() {
        if match_exprs(&input_chars, &exprs, i).is_some() {
            return Ok(true);
        }
    }

    // Handle empty string case: when input_line.len() == 0, the for loop above doesn't execute
    // We still need to check if the pattern can match an empty string (e.g., pattern like "^$")
    if match_exprs(&input_chars, &exprs, 0).is_some() {
        return Ok(true);
    }
    Ok(false)
}

/// Entry point for the grep program.
///
/// # Usage
///
/// ```sh
/// echo <input_text> | grep -E <pattern>
/// ```
///
/// # Exit Codes
///
/// * `0` - Pattern matched
/// * `1` - Pattern did not match, or invalid arguments
fn main() -> Result<(), GrepError> {
    eprintln!("Logs from your program will appear here!");

    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();
    let mut input_line = String::new();

    _ = io::stdin()
        .read_line(&mut input_line)
        .map_err(|_| GrepError::IoError);
    let regex = Regex::new(&pattern)?;
    if regex.matches(&input_line)? {
        process::exit(0)
    } else {
        process::exit(1)
    }
}
