#include <stack>
#define PRINT(text) i.print(text); break;

enum parsedtype {
    //openings
    THEN,

    //closures
    END
};

struct tokensinfo {
    uint current = 0, pscope = 0, qscope = 0;
	std::string filename, lastvariable;
	std::vector<token> tokens;
    std::stack<parsedtype> queue = std::stack<parsedtype>();
    FILE *output;
	
	tokensinfo(std::vector<token> tokens, std::string filename, FILE *output) :
		tokens(tokens),
		filename(filename),
        output(output)
	{}

    inline void print(std::string text) {
        fprintf(output, "%s", text.c_str());
    }

    void error(std::string message) {
        fclose(output);
        throw std::string("Error in file \"" + filename + "\" at line [" + std::to_string(tokens[current].line) + "]: " + message + ".");
    }

    inline void unexpected(std::string character) {
        error("Unexpected token '" + character + "'");
    }

    inline bool ended() {
        return tokens[current].type == EOF;
    }

    inline token advance() {
        return tokens[current++];
    }

    inline token peek() {
        return tokens[current + 1];
    }

    inline token previous() {
        return tokens[current - 2];
    }

    inline bool isLiteral(tokentype check) {
        return check == IDENTIFIER || check == STRING || check == NUMBER || check == TRUE || check == FALSE || check == NIL;
    }

    void parseComparison(std::string comparison, std::string toprint) {
        if (!isLiteral(previous().type)) error(comparison);
        print(" " + toprint + " ");
    }
};

void ParseTokens(std::vector<token> tokens, std::string filename, FILE *output) {
    tokensinfo i(tokens, filename, output);
    while(!i.ended()) {
        token t = i.advance();
        if (t.type == EOF) i.unexpected("<eof>");
        switch (t.type) {
            case ROUND_BRACKET_OPEN: {
                //REMEMBER TO ADD THE CHECKS
                i.pscope++;
                PRINT("(")
            }
            case ROUND_BRACKET_CLOSED: {
                if (i.pscope == 0) i.unexpected(")");
                i.pscope--;
                PRINT(")")
            }
            case SQUARE_BRACKET_OPEN: {
                i.qscope++;
                PRINT("[")
            }
            case SQUARE_BRACKET_CLOSED: {
                if (i.pscope == 0) i.unexpected("]");
                i.qscope--;
                PRINT("]")
            }
            case CURLY_BRACKET_OPEN: {
                if (i.queue.empty()) i.unexpected("{");
                parsedtype type = i.queue.top();
                i.queue.pop();
                switch (type) {
                    case THEN: {
                        i.print(" then\n");
                        i.queue.push(END);
                        break;
                    }
                    default: i.unexpected("{");
                }
                break;
            }
            case CURLY_BRACKET_CLOSED: {
                if (i.queue.empty()) i.unexpected("}");
                parsedtype type = i.queue.top();
                i.queue.pop();
                switch (type) {
                    case END: {
                        i.print("\nend\n");
                        break;
                    }
                    default: i.unexpected("}");
                }
                break;
            }
            case COMMA: PRINT(",")
            case DOT: PRINT(".")
            case SEMICOLON: PRINT(";\n")
            case NOT: PRINT(" not ")
            case AND: PRINT(" and ")
            case OR: PRINT(" or ")
            case DOLLAR: {
                if (i.lastvariable.empty()) i.unexpected("$");
                PRINT(i.lastvariable)
            }
            case PLUS: PRINT("+")
            case MINUS: PRINT("-")
            case STAR: PRINT("*")
            case SLASH: PRINT("/")
            case CARET: PRINT("^")
            case DEFINE: {
                i.lastvariable = i.previous().lexeme;
                PRINT(" = ")
            }
            case DEFINEIF: {
                i.lastvariable = i.previous().lexeme;
                PRINT(" = " + i.lastvariable + " and ")
            }
            case INCREASE: {
                i.lastvariable = i.previous().lexeme;
                PRINT(" = " + i.lastvariable + " + ")
            }
            case DECREASE: {
                i.lastvariable = i.previous().lexeme;
                PRINT(" = " + i.lastvariable + " - ")
            }
            case MULTIPLY: {
                i.lastvariable = i.previous().lexeme;
                PRINT(" = " + i.lastvariable + " * ")
            }
            case DIVIDE: {
                i.lastvariable = i.previous().lexeme;
                PRINT(" = " + i.lastvariable + " / ")
            }
            case EXPONENTIATE: {
                i.lastvariable = i.previous().lexeme;
                PRINT(" = " + i.lastvariable + " ^ ")
            }
            case EQUAL: case BIGGER_EQUAL: case SMALLER_EQUAL: case BIGGER: case SMALLER: {
                i.parseComparison(t.lexeme, t.lexeme);
                break;
            }
            case NOT_EQUAL: {
                i.parseComparison("!=", "~=");
                break;
            }
            case IDENTIFIER: PRINT(t.lexeme)
            case NUMBER: PRINT(t.literal)
            case STRING: PRINT("[[" + t.literal + "]]")
            case IF: {
                i.print("if ");
                i.queue.push(THEN);
                break;
            }
            case LOCAL: PRINT("local ")
        }
    }
    if (i.pscope > 0) i.error("Expected token ')' before '<eof>'");
    if (i.qscope > 0) i.error("Expected token ']' before '<eof>'");
}