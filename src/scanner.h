#ifndef clox_scanner_h
#define clox_scanner_h

/* Token types.
 *
 * Each TokenType represents a distinctly identifiable piece of text within the
 * source code. These are assigned by the Scanner class as it processes the
 * source in order to provide a consistently identifiable set of Tokens for
 * the Parser.
 *
 * C enum names are in the global namespace, so the TOKEN_ prefix is added for
 * uniqueness.
 */
typedef enum {
  // Single-character tokens.
  TOKEN_LEFT_PAREN, TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACE, TOKEN_RIGHT_BRACE,
  TOKEN_COMMA, TOKEN_DOT, TOKEN_MINUS, TOKEN_PLUS,
  TOKEN_SEMICOLON, TOKEN_SLASH, TOKEN_STAR,
  // One or two character tokens.
  TOKEN_BANG, TOKEN_BANG_EQUAL,
  TOKEN_EQUAL, TOKEN_EQUAL_EQUAL,
  TOKEN_GREATER, TOKEN_GREATER_EQUAL,
  TOKEN_LESS, TOKEN_LESS_EQUAL,
  // Literals.
  TOKEN_IDENTIFIER, TOKEN_STRING, TOKEN_NUMBER,
  // Keywords.
  TOKEN_AND, TOKEN_CLASS, TOKEN_ELSE, TOKEN_FALSE,
  TOKEN_FOR, TOKEN_FUN, TOKEN_IF, TOKEN_NIL, TOKEN_OR,
  TOKEN_PRINT, TOKEN_RETURN, TOKEN_SUPER, TOKEN_THIS,
  TOKEN_TRUE, TOKEN_VAR, TOKEN_WHILE,

  TOKEN_ERROR, TOKEN_EOF
} TokenType;

/* Scanner Token.
 *
 * A Token represents a "chunk" of text within the processed source code. These
 * are returned by the Scanner as a stream which are then consumed by the
 * Compiler in order to generate the bytecode representation.
 *
 * Rather than storing the lexeme within the Token itself, this stores the
 * pointer to the start char in the source provided to initScanner(), and it's
 * length. This can then be used to retrieve the lexeme text from the source at
 * any time.
 */
typedef struct {
    TokenType type;  // The type of Token being identified, see TokenType.
    const char* start;  // Pointer to the start of the lexeme in the source text
    int length;  // Length of the lexeme
    int line;  // Line number in the source where this Token was scanned
} Token;

void initScanner(const char* source);
Token scanToken();

#endif
