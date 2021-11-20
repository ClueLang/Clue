#include <stack>
#define PRINT(text) i.print(text); break;
#define CODEBLOCK(start, type) {i.print(start); i.queue.push(type); break;}

enum parsedtype {
    //openings
    THEN, NEXTLINE, FOR_LOOP, WHILE_LOOP,

    //closures
    IF_END, END, TABLE_END
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
        return tokens[current];
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

    void parseVariable() {
        //if (current == 0)
        if (tokens[current - 2].type == DOLLAR) return;
        lastvariable.clear();
        int i = current - 2;
        while (i > 0 && tokens[i].type != SEMICOLON && tokens[i].type != CURLY_BRACKET_OPEN && tokens[i].type != CURLY_BRACKET_CLOSED && tokens[i].type != LOCAL) {
            lastvariable.insert(0, tokens[i].lexeme);
            i--;
        }
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
                PRINT("[ ")
            }
            case SQUARE_BRACKET_CLOSED: {
                if (i.qscope == 0) i.unexpected("]");
                i.qscope--;
                PRINT(" ]")
            }
            case CURLY_BRACKET_OPEN: {
                if (i.queue.empty()) i.unexpected("{");
                parsedtype type = i.queue.top();
                i.queue.pop();
                switch (type) {
                    case THEN: CODEBLOCK(" then\n", IF_END)
                    case NEXTLINE: CODEBLOCK("\n", END)
                    case FOR_LOOP: case WHILE_LOOP: CODEBLOCK(" do\n", END)
                    default: i.unexpected("{");
                }
                break;
            }
            case CURLY_BRACKET_CLOSED: {
                if (i.queue.empty()) i.unexpected("}");
                parsedtype type = i.queue.top();
                i.queue.pop();
                switch (type) {
                    case IF_END: {
                        tokentype check = i.tokens[i.current].type;
                        if (check == ELSEIF) {
                            i.advance();
                            CODEBLOCK("\nelseif ", THEN)
                        } else if (check == ELSE) {
                            i.advance();
                            CODEBLOCK("\nelse\n", NEXTLINE)
                        }
                        //if there is not an elseif just do what END does
                    }
                    case END: {
                        i.print("\nend\n");
                        break;
                    }
                    case TABLE_END: {
                        i.print("\n}\n");
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
            case HASHTAG: PRINT("#")
            case DEFINE: {
                i.parseVariable();
                PRINT(" = ")
            }
            case DEFINEIF: {
                i.parseVariable();
                PRINT(" = " + i.lastvariable + " and ")
            }
            case INCREASE: {
                i.parseVariable();
                PRINT(" = " + i.lastvariable + " + ")
            }
            case DECREASE: {
                i.parseVariable();
                PRINT(" = " + i.lastvariable + " - ")
            }
            case MULTIPLY: {
                i.parseVariable();
                PRINT(" = " + i.lastvariable + " * ")
            }
            case DIVIDE: {
                i.parseVariable();
                PRINT(" = " + i.lastvariable + " / ")
            }
            case EXPONENTIATE: {
                i.parseVariable();
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
            case LAMBDA: {
                if (i.previous().type == IDENTIFIER) i.print(" = ");
                i.print("function");
                i.queue.push(NEXTLINE);
                token check = i.peek();
                if (check.type == CURLY_BRACKET_OPEN) {
                    i.print("()");
                } else if (check.type != ROUND_BRACKET_OPEN) i.unexpected(check.lexeme);
                break;
            }
            case IDENTIFIER: PRINT(t.lexeme)
            case NUMBER: PRINT(t.literal)
            case STRING: PRINT("[[" + t.literal + "]]")
            case TABLE: CODEBLOCK("{\n", TABLE_END)
            case DO: CODEBLOCK("do", NEXTLINE)
            case IF: CODEBLOCK("if ", THEN)
            case ELSEIF: i.unexpected("elseif");
            case ELSE: i.unexpected("else");
            case FOR: CODEBLOCK("for ", FOR_LOOP)
            case OF: {
                token table = i.peek();
                if (i.queue.top() != FOR_LOOP || table.type != IDENTIFIER) i.unexpected("of");
                i.print(" in pairs(" + table.lexeme + ")");
                i.advance();
                break;
            }
            case IN: {
                token array = i.peek();
                if (i.queue.top() != FOR_LOOP || array.type != IDENTIFIER) i.unexpected("of");
                i.print(" in ipairs(" + array.lexeme + ")");
                i.advance();
                break;
            }
            case WITH: {
                if (i.queue.top() != FOR_LOOP) i.unexpected("with");
                i.print(" in ");
                break;
            }
            case LOCAL: PRINT("local ")
        }
    }
    if (i.pscope > 0) i.error("Expected token ')' before '<eof>'");
    if (i.qscope > 0) i.error("Expected token ']' before '<eof>'");
}
