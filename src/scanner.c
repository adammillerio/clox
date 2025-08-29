#include "scanner.h"

#include <stdio.h>  // IWYU pragma: keep
#include <string.h>

#include "common.h"  // IWYU pragma: keep

/* Lox Scanner.
 *
 * This scans a given source text into a stream of Tokens, to be used by the
 * Parser/Compiler to generate bytecode.
 */
typedef struct {
    const char* start;  // Pointer to the beginning of the current lexeme being
                        // scanned.
    const char* current;  // Pointer to the current character.
    int line;             // Current line being scanned, for error reporting.
} Scanner;

// Lox Scanner "Singleton" global variable.
// All functions in this module will reference this global Scanner.
Scanner scanner;

/* Initialize the Scanner
 *
 * This initializes both the start and current pointers to the start of the
 * source string provided.
 *
 * @param source string containing the Lox source to scan
 */
void initScanner(const char* source) {
    scanner.start = source;
    scanner.current = source;
    scanner.line = 1;
}

/* Check if a char is alphabetic or an underscore.
 *
 * @param c char to check
 *
 * @return whether or not char is in [a-zA-Z_]
 */
static bool isAlpha(char c) {
    // [a-zA-Z_]
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

/* Check if a char is a digit.
 *
 * @param c char to check
 *
 * @return whether or not char is in [0-9]
 */
static bool isDigit(char c) { return c >= '0' && c <= '9'; }

/* Check if a the Scanner is at the end of the source string
 *
 * This relies on the null char \0 which is added when loading a source text.
 *
 * @return whether or not the Scanner is at the end
 */
static bool isAtEnd() { return *scanner.current == '\0'; }

/* Retrieve the char at the Scanner's current position and increment it.
 *
 * @return retrieved char
 */
static char advance() {
    scanner.current++;
    return scanner.current[-1];
}

/* Return the char at the current index without consuming it.
 *
 * @return char at the current index
 */
static char peek() { return *scanner.current; }

/* Return the char at the current index + 1 without consuming it.
 *
 * @return char at the current index + 1, or \0 if at end of source string
 */
static char peekNext() {
    if (isAtEnd()) return '\0';

    return scanner.current[1];
}

/* Check for a given char at the current index and increment if found.
 *
 * If there is a match, the current index will be incremented. This is used for
 * parsing multi-character lexemes such as != and ==.
 *
 * @param expected expected char
 *
 * @return whether or not the char at the now previous index matched the
 * expected char
 */
static bool match(char expected) {
    // End or no match, don't advance
    if (isAtEnd()) return false;
    if (*scanner.current != expected) return false;

    // Matched, advance
    scanner.current++;
    return true;
}

/* Create a Token from a portion of the scanned source.
 *
 * This stores a pointer to the start of the lexeme, then uses the Scanner's
 * current/start pointers to calculate the length, as well as the line that it
 * was encountered. The pointer and value can be used to reconstruct the lexeme
 * from the original source string, rather than allocating and storing it in
 * the Token.
 *
 * @param type the type of token to create from the current lexeme
 *
 * @return the Token created from the current lexeme
 */
static Token makeToken(TokenType type) {
    Token token;
    token.type = type;

    // Store reference to start of this lexeme
    token.start = scanner.start;
    // Calculate the length of the lexeme via the current/start pointers
    // The string itself is not returned, so these are used to retrieve the
    // lexeme itself from the source text string
    token.length = (int)(scanner.current - scanner.start);
    token.line = scanner.line;

    return token;
}

/* Create an error Token.
 *
 * If an error is encountered during scanning, a token of type TOKEN_ERROR will
 * be added to the token stream, with a pointer to an error message rather than
 * to the source string. Because this function is only ever called with string
 * literals ie "Unexpected error", which are static, they will live long enough
 * to be accessed and reported to the user upstream.
 *
 * @param message error message string to report to the user
 *
 * @return a signal Token of type TOKEN_ERROR with the error details
 */
static Token errorToken(const char* message) {
    Token token;
    token.type = TOKEN_ERROR;

    // Point to the provided error message instead. This function is only
    // ever called with string literals, which are static.
    token.start = message;
    token.length = (int)strlen(message);

    token.line = scanner.line;

    return token;
}

/* Skip through whitespace in the Scanner source.
 *
 * This is called before scanning a new Token and skips all whitespace and line
 * comments. If anything else is encountered, it will return without consuming,
 * so that it becomes the first char in the next lexeme.
 */
static void skipWhitespace() {
    for (;;) {
        char c = peek();

        switch (c) {
            case ' ':
            case '\r':
            case '\t':
                advance();
                break;
            case '\n':
                scanner.line++;
                advance();
                break;
            case '/':
                if (peekNext() == '/') {
                    // A comment goes until the end of the line.
                    while (peek() != '\n' && !isAtEnd()) advance();
                } else {
                    return;
                }

                break;
            default:
                return;
        }
    }
}

/* Check if the upcoming source matches a given string.
 *
 * Given a start index relative to the Scanner's current position, which
 * represents the first char to check, a length of chars, and a string, this
 * will compare the next length chars against the provided rest string,
 * returning a Token of the given type if matched. This implements the traversal
 * down the scanning trie in identifierType for paths which have no more
 * branching.
 *
 * @param start start position of the "rest" of the keyword
 * @param length length in chars to check against the "rest"
 * @param rest string with the "rest" of the keyword to check
 * @param type the type of token to create if matched
 *
 * @return the matched keyword type if found, or TOKEN_IDENTIFIER otherwise,
 * representing a user defined identifier.
 */
static TokenType checkKeyword(int start, int length, const char* rest,
                              TokenType type) {
    // V
    // and
    // scanner.start = 0
    // scanner.current = 1
    // start = 1, length = 2, rest = "nd", type = TOKEN_AND
    // The current lexeme's size is equal to start + length
    // Python: len("and") == 3
    if (scanner.current - scanner.start == start + length &&
        // Starting at 1 (0 + 1), compare the next 2 (length) chars to the right
        // hand array "nd" (rest)
        // Python: "and"[1:3] == "nd"
        // https://en.cppreference.com/w/cpp/string/byte/memcmp
        memcmp(scanner.start + start, rest, length) == 0) {
        // and matched in source, return TOKEN_AND
        return type;
    }

    return TOKEN_IDENTIFIER;
}

/* Determine the type of a scanned identifier Token.
 *
 * This is called whenever the Scanner encounters a char(s) in the range
 * [a-zA-z_] and implements a trie which identifies all reserved keywords within
 * the Lox language. If no keyword is matched, the scanned lexeme is returned as
 * a token of type TOKEN_IDENTIFIER, indicating a user defined value like a var.
 *
 * For more info and a graphical representation of the trie, see Section 16.4
 * "Identifiers and Keywords".
 *
 * Or the wikipedia article: https://en.wikipedia.org/wiki/Trie
 *
 * @return the matched keyword type if found, or TOKEN_IDENTIFIER otherwise,
 * representing a user defined identifier.
 */
static TokenType identifierType() {
    switch (scanner.start[0]) {
        case 'a':
            // and
            return checkKeyword(1, 2, "nd", TOKEN_AND);
        case 'c':
            // class
            return checkKeyword(1, 4, "lass", TOKEN_CLASS);
        case 'e':
            // else
            return checkKeyword(1, 3, "lse", TOKEN_ELSE);
        case 'f':
            // There is a character after f
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'a':
                        // false
                        return checkKeyword(2, 3, "lse", TOKEN_FALSE);
                    case 'o':
                        // for
                        return checkKeyword(2, 1, "r", TOKEN_FOR);
                    case 'u':
                        // fun
                        return checkKeyword(2, 1, "n", TOKEN_FUN);
                }
            }
            break;
        case 'i':
            // if
            return checkKeyword(1, 1, "f", TOKEN_IF);
        case 'n':
            // nil
            return checkKeyword(1, 2, "il", TOKEN_NIL);
        case 'o':
            // or
            return checkKeyword(1, 1, "r", TOKEN_OR);
        case 'p':
            // print
            return checkKeyword(1, 4, "rint", TOKEN_PRINT);
        case 'r':
            // return
            return checkKeyword(1, 5, "eturn", TOKEN_RETURN);
        case 's':
            // super
            return checkKeyword(1, 4, "uper", TOKEN_SUPER);
        case 't':
            // There is a character after t
            if (scanner.current - scanner.start > 1) {
                switch (scanner.start[1]) {
                    case 'h':
                        // this
                        return checkKeyword(2, 2, "is", TOKEN_THIS);
                    case 'r':
                        // true
                        return checkKeyword(2, 2, "ue", TOKEN_TRUE);
                }
            }
            break;
        case 'v':
            // var
            return checkKeyword(1, 2, "ar", TOKEN_VAR);
        case 'w':
            // while
            return checkKeyword(1, 4, "hile", TOKEN_WHILE);
    }

    return TOKEN_IDENTIFIER;
}

/* Scan and make an identifier Token.
 *
 * This consumes all characters in the range [a-zA-Z0-9_] and then uses a trie
 * implemented in identifierType to determine if the scanned lexeme is a
 * reserved Lox keyword ie "and" or a user identifier like a variable.
 *
 * @return Token with the matched keyword type if found, or a Token of type
 * TOKEN_IDENTIFIER otherwise, representing a user defined identifier.
 */
static Token identifier() {
    while (isAlpha(peek()) || isDigit(peek())) advance();

    return makeToken(identifierType());
}

/* Scan and make a number Token.
 *
 * This scans in a digit(s), an optional ".", and an optional set of digit(s)
 * to represent the fractional part, then returns a Token of type TOKEN_NUMBER
 * to indicate a number literal.
 *
 * @return token of type TOKEN_NUMBER with the scanned number literal
 */
static Token number() {
    while (isDigit(peek())) advance();

    // Look for a fractional part
    if (peek() == '.' && isDigit(peekNext())) {
        // Consume the "."
        advance();

        while (isDigit(peek())) advance();
    }

    return makeToken(TOKEN_NUMBER);
}

/* Scan and make a string Token.
 *
 * This scans in all characters (including newlines) within a set of double
 * quotes and returns a Token of type TOKEN_STRING to inidicate a string
 * literal.
 *
 * @return token of type TOKEN_STRING with the scanned string literal
 */
static Token string() {
    while (peek() != '"' && !isAtEnd()) {
        if (peek() == '\n') scanner.line++;
        advance();
    }

    if (isAtEnd()) return errorToken("Unterminated string.");

    // The closing quote
    advance();
    return makeToken(TOKEN_STRING);
}

/* Scan and return a Token from the Lox source.
 *
 * This seeks forward in the source, skipping all whitespace, then continues to
 * scan until a Token of some type is identified. The matched Token is then
 * returned so that it can be processed in the token stream.
 *
 * @return the scanned token
 */
Token scanToken() {
    skipWhitespace();

    // Set start pointer to current, since this is the start of a new Token
    scanner.start = scanner.current;

    if (isAtEnd()) return makeToken(TOKEN_EOF);

    char c = advance();
    if (isAlpha(c)) return identifier();
    if (isDigit(c)) return number();

    switch (c) {
        // Single character tokens.
        case '(':
            return makeToken(TOKEN_LEFT_PAREN);
        case ')':
            return makeToken(TOKEN_RIGHT_PAREN);
        case '{':
            return makeToken(TOKEN_LEFT_BRACE);
        case '}':
            return makeToken(TOKEN_RIGHT_BRACE);
        case ';':
            return makeToken(TOKEN_SEMICOLON);
        case ',':
            return makeToken(TOKEN_COMMA);
        case '.':
            return makeToken(TOKEN_DOT);
        case '-':
            return makeToken(TOKEN_MINUS);
        case '+':
            return makeToken(TOKEN_PLUS);
        case '/':
            return makeToken(TOKEN_SLASH);
        case '*':
            return makeToken(TOKEN_STAR);
        // One or two character tokens.
        case '!':
            return makeToken(match('=') ? TOKEN_BANG_EQUAL : TOKEN_BANG);
        case '=':
            return makeToken(match('=') ? TOKEN_EQUAL_EQUAL : TOKEN_EQUAL);
        case '<':
            return makeToken(match('=') ? TOKEN_LESS_EQUAL : TOKEN_LESS);
        case '>':
            return makeToken(match('=') ? TOKEN_GREATER_EQUAL : TOKEN_GREATER);
        case '"':
            return string();
    }

    return errorToken("Unexpected character.");
}
