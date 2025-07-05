use std::env;
use std::io;
use std::process;

/// Checks if the input line matches the given pattern.
///
/// Supported patterns:
/// - A single character: returns true if the character is present in the input line.
/// - `\d`: returns true if the input line contains at least one ASCII digit.
/// - '\w' returns true if the input contains at least one of a-z, A-Z, 0-9 or _.
///
/// # Arguments
///
/// * `input_line` - The line of text to search within.
/// * `pattern` - The pattern to match against the input line.
///
/// # Returns
///
/// * `true` if the pattern matches the input line, otherwise `false`.
fn match_pattern(input_line: &str, pattern: &str) -> bool {
    match pattern {
        p if p.len() == 1 => input_line.contains(p),
        r"\d" => input_line.chars().any(|char| char.is_ascii_digit()),
        r"\w" => input_line.chars().any(|char| char.is_alphanumeric()),
        p if p.len() >= 3
            && p.as_bytes().first() == Some(&b'[')
            && p.as_bytes().last() == Some(&b']') =>
        {
            p.chars()
                .any(|pchar| input_line.chars().any(|ilchar| ilchar == pchar))
        }
        _ => false,
    }
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
