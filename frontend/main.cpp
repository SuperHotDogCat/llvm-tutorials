#include <string>
#include <cstdio>
#include <cctype>
// The lexer returns tokens [0-255] if it is an unknown character (ASCII character), otherwise one
// of these for known things.
enum Token {
    tok_eof = -1,
    // commands
    tok_def = -2,
    tok_extern = -3,
    // primary
    tok_identifier = -4,
    tok_number = -5,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal; // Filled in if tok_number

// getNextTok - Return the next token from the stdin
static int getNextTok() {
    static int lastChar = ' ';
    while (std::isspace(lastChar)) {
        lastChar = std::getchar();
    }

    if (std::isalpha(lastChar)) {
        // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = lastChar;
        while (std::isalnum(lastChar = std::getchar())){
            IdentifierStr += lastChar;
        }
        
        if (IdentifierStr == "def")
            return tok_def;
        if (IdentifierStr == "extern")
            return tok_extern;
        return tok_identifier;
    }

    if (std::isdigit(lastChar) || lastChar == '.') {
        // misunderstanding some digits e.g. 1.23.64
        // but this stays now.
        std::string NumStr;
        do {
            NumStr += lastChar;
            lastChar = std::getchar();
        } while (std::isdigit(lastChar) || lastChar == '.');
        NumVal = std::strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    if (lastChar == '#') {
        // comment until the end of line.
        do {
            lastChar = std::getchar();
        } while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');

        if (lastChar != EOF) 
            return getNextTok();
    }

    if (lastChar == EOF)
        return tok_eof;
    // Otherwise, just return the character as its ascii value.
    int thisChar = lastChar;
    lastChar = std::getchar();
    return thisChar;
}
