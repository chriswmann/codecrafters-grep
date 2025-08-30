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
}

#[derive(Debug, Clone)]
enum Expr {
    Digit,
    Whitespace,
    WordCharacter,
    Range(String),
    Literal(String),
    StartAnchor,
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
            _ => {
                // Collect consecutive literal characters
                let start_pos = i;
                let mut end_pos = i + chr.len_utf8();

                while let Some((pos, chr)) = chars.peek() {
                    if matches!(chr, '\\' | '[') {
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
            // ^ matches only at the beginning of the string
            if start_pos == 0 {
                Some(start_pos)
            } else {
                None
            }
        }
    }
}

fn matches_range(chr: char, range_str: &str) -> bool {
    // Remove the brackets
    let inner = &range_str[1..range_str.len() - 1];

    // Handle negative characters
    if inner.starts_with('^') {
        return !inner.contains(chr);
    }

    // Handle simple character lists, like [abc]
    if !inner.contains('-') {
        return inner.contains(chr);
    }

    // Handle ranges, e.g. [a-z] or [1-3]
    let parts: Vec<&str> = inner.split('-').collect();
    if parts.len() == 2 && parts[0].len() == 1 && parts[1].len() == 1 {
        let start = parts[0].chars().next().unwrap();
        let end = parts[1].chars().next().unwrap();
        return chr >= start && chr <= end;
    }

    // If we've got anything more complicated, just fall back to a character list
    inner.contains(chr)
}

fn get_char_at(input: &str, start_pos: usize) -> Option<char> {
    if start_pos < input.len() {
        input.chars().nth(start_pos)
    } else {
        None
    }
}

fn match_pattern(input_line: &str, pattern: &str) -> bool {
    let tokens = tokenise(pattern);
    let expr = parse(&tokens);

    // Check if the pattern starts with a start anchor
    let has_start_anchor = matches!(tokens.first(), Some(Token::StartAnchor));

    if has_start_anchor {
        // If there's a start anchor, we only want to match from the start of the string
        return match_expr(input_line, &expr, 0).is_some();
    } else {
        // Try matching at every position in the input
        for i in 0..input_line.len() {
            if let Some(_) = match_expr(input_line, &expr, i) {
                return true;
            }
        }
    }

    // Cover empty string case - if input_line is empty, the for loop becomes for i in 0..0, which is empty
    // so the loop doesn't run

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
