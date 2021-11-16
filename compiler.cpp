#include <stack>

enum parsedtype {
    //openings
    THEN,

    //closures
    END
};

struct tokensinfo {
    uint line = 1, current = 0;
	std::string filename;
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
        throw std::string("Unexpected token '" + message + "'.");
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
        return tokens[current - 1];
    }

    void parse() {
        token t = advance();
        if (t.type == EOF) error("<eof>");
        switch (t.type) {
            case CURLY_BRACKET_OPEN: {
                if (queue.empty()) error("{");
                parsedtype type = queue.top();
                queue.pop();
                switch (type) {
                    case THEN: {
                        print(" then\n");
                        line++;
                        queue.push(END);
                        break;
                    }
                    default: error("{");
                }
                break;
            }
            case CURLY_BRACKET_CLOSED: {
                if (queue.empty()) error("}");
                parsedtype type = queue.top();
                queue.pop();
                switch (type) {
                    case END: {
                        print("\nend\n");
                        line += 2;
                        break;
                    }
                    default: error("}");
                }
                break;
            }
            case EQUAL: case BIGGER_EQUAL: case SMALLER_EQUAL: case BIGGER: case SMALLER: {
                tokentype check = previous().type;
                if (check != IDENTIFIER && check != STRING && check != NUMBER && check != TRUE && check != FALSE && check != NIL)
                    error(t.lexeme);
                print(t.lexeme);
                break;
            }
            case NOT_EQUAL: {
                tokentype check = previous().type;
                if (check != IDENTIFIER && check != STRING && check != NUMBER && check != TRUE && check != FALSE && check != NIL)
                    error("!=");
                print("~=");
                break;
            }
            case IDENTIFIER: {
                print(t.lexeme);
                parse();
                break;
            }
            case IF: {
                print("if ");
                queue.push(THEN);
                parse();
                break;
            }
        }
    }
};