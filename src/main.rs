// See https://claude.ai/chat/f8c552a7-d4a5-4394-a3f2-cf1c59892b2e
#![warn(clippy::all)]
use std::env;
use std::io;
use std::process;

#[derive(Debug, Clone)]
enum Token {
    Digit,
    Whitespace,
    Range(String),
    Literal(String),
    WordCharacter,
    StartAnchor,
    EndAnchor,
}

#[derive(Debug, Clone)]
enum Expr {
    Digit,
    Whitespace,
    WordCharacter,
    Range(String),
    Literal(String),
    StartAnchor,
    EndAnchor,
    Sequence(Vec<Expr>),
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
/// - `Token::Range(String)`: Represents a character class or range, e.g., `[0-9]`, `[a-z]`.
/// - `Token::Literal(String)`: Represents literal substrings or characters.
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
fn tokenise(input: &str) -> Vec<Token> {
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
                        '\\' => {
                            // Handle literal backslack - '\\' becomes a literal '\'
                            tokens.push(Token::Literal("\\".into()));
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
            '[' => {
                // Parse a character class like [0-9]
                // Look for the closing bracket
                let remainder = &input[i..];
                if let Some(end_pos) = remainder.find(']') {
                    let range = &remainder[..end_pos + 1];
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
                if chars.peek() == None {
                    tokens.push(Token::EndAnchor);
                } else {
                    tokens.push(Token::Literal("$".to_string()));
                }
            }
            _ => {
                // Collect consecutive literal characters
                let start_pos = i;
                let mut end_pos = i + chr.len_utf8();

                while let Some((pos, chr)) = chars.peek() {
                    // If we hit one of these chars, we need to break out so we can handle them in the
                    // outer loop, as they could indicate a special character.
                    if matches!(chr, '\\' | '[' | '$') {
                        break;
                    }
                    end_pos = pos + chr.len_utf8();
                    chars.next();
                }
                let literal = &input[start_pos..end_pos];
                tokens.push(Token::Literal(literal.to_string()));
            }
        }
    }
    tokens
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
/// Returns an `Expr::Sequence` containing a vector of expressions, where each expression
/// corresponds to a token from the input. The sequence represents the entire parsed
/// regex pattern as a single evaluatable expression.
///
/// # Token to Expression Mapping
///
/// * `Token::Digit` → `Expr::Digit` - Matches any digit character (0-9)
/// * `Token::Whitespace` → `Expr::Whitespace` - Matches whitespace characters
/// * `Token::Range(r)` → `Expr::Range(r)` - Matches character ranges like [a-z]
/// * `Token::Literal(l)` → `Expr::Literal(l)` - Matches literal string characters
/// * `Token::WordCharacter` → `Expr::WordCharacter` - Matches word characters (\w)
///
/// # Example
///
/// ```
/// let tokens = vec![Token::Literal("hello".to_string()), Token::Digit];
/// let expr = parse(&tokens);
/// // Returns: Expr::Sequence([Expr::Literal("hello"), Expr::Digit])
/// ```
fn parse(tokens: &[Token]) -> Expr {
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
        };
        exprs.push(expr);
    }
    Expr::Sequence(exprs)
}

/// A recursive function that tries to match an expression at a specific position in the input string
///
/// Returns Some(new_position) if match succeeds, None if it fails
/// Handles each expression type differently
fn match_expr(input: &str, expr: &Expr, start_pos: usize) -> Option<usize> {
    match expr {
        Expr::Digit => {
            let chr = get_char_at(input, start_pos)?;
            if chr.is_ascii_digit() {
                Some(start_pos + 1)
            } else {
                None
            }
        }
        Expr::Whitespace => {
            let chr = get_char_at(input, start_pos)?;
            if chr.is_whitespace() {
                Some(start_pos + 1)
            } else {
                None
            }
        }
        Expr::Range(range_str) => {
            let chr = get_char_at(input, start_pos)?;
            if matches_range(chr, range_str) {
                Some(start_pos + 1)
            } else {
                None
            }
        }
        Expr::Literal(literal_str) => {
            if input[start_pos..].starts_with(literal_str) {
                Some(start_pos + literal_str.len()) // Need to make sure we move past the matched literal
            } else {
                None
            }
        }
        Expr::Sequence(exprs) => {
            let mut pos = start_pos; // Track current position
            for expr in exprs {
                // At each position, loop through exprs for a match
                match match_expr(input, expr, pos) {
                    // Recursion
                    Some(new_pos) => pos = new_pos, // Success! Move to the next position in the input string
                    None => return None,            // Failure: entire sequence fails
                }
            }
            Some(pos) // All expressions matched successfully
        }
        Expr::WordCharacter => {
            let char = get_char_at(input, start_pos)?;
            if char.is_ascii_alphanumeric() || char == '_' {
                Some(start_pos + 1)
            } else {
                None
            }
        }
        Expr::StartAnchor => {
            // ^ asserts we're at the beginning - it doesn't consume characters
            // Returns the same position if we're at start, None otherwise
            if start_pos == 0 {
                Some(start_pos)
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
fn get_char_at(input: &str, pos: usize) -> Option<char> {
    if pos < input.len() {
        input.chars().nth(pos)
    } else {
        None
    }
}

/// Determines if a regex pattern matches anywhere in the input string.
///
/// This function implements the main matching strategy, handling anchors specially:
/// - Start anchors (^) constrain matching to only begin at position 0
/// - End anchors ($) are handled by the match_expr logic, ensuring matches end at string end
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
fn match_pattern(input_line: &str, pattern: &str) -> bool {
    let tokens = tokenise(pattern);
    let expr = parse(&tokens);

    // Check if the pattern starts with a start anchor
    let has_start_anchor = matches!(tokens.first(), Some(Token::StartAnchor));

    if has_start_anchor {
        // Start anchor means the pattern must match from the very beginning
        // No need to try other positions - this is how ^ works in regex
        return match_expr(input_line, &expr, 0).is_some();
    } else {
        // Without start anchor, try matching at every position in the input
        // This implements the standard regex behavior of finding matches anywhere
        for i in 0..input_line.len() {
            if let Some(_) = match_expr(input_line, &expr, i) {
                return true;
            }
        }
    }

    // Handle empty string case: when input_line.len() == 0, the for loop above doesn't execute
    // We still need to check if the pattern can match an empty string (e.g., pattern like "^$")
    if let Some(_) = match_expr(input_line, &expr, 0) {
        return true;
    }
    false
}

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    // You can use print statements as follows for debugging, they'll be visible when running tests.
    eprintln!("Logs from your program will appear here!");

    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();
    let mut input_line = String::new();

    io::stdin().read_line(&mut input_line).unwrap();

    // Uncomment this block to pass the first stage
    if match_pattern(&input_line, &pattern) {
        process::exit(0)
    } else {
        process::exit(1)
    }
}
