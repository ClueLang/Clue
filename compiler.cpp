#include <stack>
#include <list>
#include <unordered_set>
#define PRINT(text) i.print(text); break;
#define CODEBLOCK(start, type) {i.print(start); i.queue.push(type); break;}

enum parsedtype {
    //openings
    THEN, NEXTLINE, FOR_LOOP, WHILE_LOOP, UNTIL_LOOP_START,

    //closures
    IF_END, END, TABLE_END, METATABLE_END, UNTIL_LOOP_END
};

struct tokensinfo {
    uint current = 0, pscope = 0, qscope = 0;
	std::string output, filename, lastvariable, repeatloopcondition;
	std::vector<token> tokens;
    std::unordered_set<std::string> labels;
    std::stack<parsedtype> queue = std::stack<parsedtype>();
    std::stack<std::list<method>> metatables;
	
	tokensinfo(std::vector<token> tokens, std::string filename, std::unordered_set<std::string> labels) :
        tokens(tokens),
        filename(filename),
        labels(labels)
    {}

    void print(std::string text) {
        output += text;
    }

    void error(std::string message) {
        throw std::string("Error in file \"" + filename + "\" at line [" + std::to_string(tokens[current].line) + "]: " + message + ".");
    }

    void unexpected(std::string character) {
        error("Unexpected token '" + character + "'");
    }

    void expectedBeforeEOF(std::string character) {
        error("Expected token '" + character + "' before '<eof>'");
    }

    bool ended() {
        return tokens[current].type == EOF;
    }

    token advance() {
        return tokens[current++];
    }

    token peek(uint offset = 0) {
        return tokens[current + offset];
    }

    token previous() {
        return tokens[current - 2];
    }

    bool isLiteral(tokentype check) {
        return check == IDENTIFIER || check == STRING || check == NUMBER || check == TRUE || check == FALSE || check == NIL;
    }

    void parseComparison(std::string comparison, std::string toprint) {
        if (!isLiteral(previous().type)) error(comparison);
        print(" " + toprint + " ");
    }

    void parseVariable() {
        if (tokens[current - 2].type == DOLLAR) return;
        lastvariable.clear();
        int i = current - 2;
        while (i > 0 && tokens[i].type != SEMICOLON && tokens[i].type != CURLY_BRACKET_OPEN && tokens[i].type != CURLY_BRACKET_CLOSED && tokens[i].type != LOCAL) {
            lastvariable.insert(0, tokens[i].lexeme);
            i--;
        }
    }

    void expect(token t, std::list<tokentype> checks) {
        tokentype check = t.type;
		for (tokentype tocheck : checks) {
            if (check == tocheck) return;
        }
        unexpected(t.lexeme);
    }
};

const std::unordered_map<std::string, std::string> methodNames = {
    {"index", "__index"},
    {"newindex", "__newindex"},
    {"mode", "__mode"},
    {"call", "__call"},
    {"metatable", "__metatable"},
    {"tostring", "__tostring"},
    {"gc", "__gc"},
    {"name", "__name"},
    {"unm", "__unm"},
    {"unary", "__unm"},
    {"add", "__add"},
    {"+", "__add"},
    {"sub", "__sub"},
    {"-", "__sub"},
    {"mul", "__mul"},
    {"*", "__mul"},
    {"div", "__div"},
    {"/", "__div"},
    {"mod", "__mod"},
    {"%", "__mod"},
    {"pow", "__pow"},
    {"^", "__pow"},
    {"concat", "__concat"},
    {"..", "__concat"},
    {"eq", "__eq"},
    {"equal", "__eq"},
    {"==", "__eq"},
    {"lt", "__lt"},
    {"less_than", "__lt"},
    {"<", "__lt"},
    {"le", "__le"},
    {"less_than_equal", "__lt"},
    {"<=", "__le"},
};

std::string ParseTokens(std::vector<token> tokens, std::string filename, std::unordered_set<std::string> labels = {});

struct method {
    std::string name, value;

    method(std::string oname, tokensinfo &i) :
        name(oname)
    {
        auto search = methodNames.find(name);
        if (search == methodNames.end()) {
            i.error("Unknown metatable event named '" + name + "'");
            return;
        }
        name = search->second;
        std::vector<token> methodTokens;
        for (uint it = i.current; i.tokens[it].type != SEMICOLON; it++) {
            if (i.tokens[it].type == EOF) i.expectedBeforeEOF(";");
            methodTokens.push_back(i.tokens[it]);
            i.advance();
        }
        i.advance();
        methodTokens.push_back(token(SEMICOLON, ";", "", 0));
        methodTokens.push_back(token(EOF, "", "", 0));
        value = ParseTokens(methodTokens, i.filename, i.labels);
        printf("%s", value);
    }
};

std::string ParseTokens(std::vector<token> tokens, std::string filename, std::unordered_set<std::string> labels) {
    tokensinfo i(tokens, filename, labels);
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
                    case UNTIL_LOOP_START: CODEBLOCK("repeat\n", UNTIL_LOOP_END)
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
                    case END: PRINT("\nend\n")
                    case TABLE_END: PRINT("\n}\n")
                    case METATABLE_END: {
                        i.print("\n}, {\n");
                        std::list<method> methods = i.metatables.top();
                        i.metatables.pop();
                        for (method m : methods) {
                            i.print(m.name + m.value);
                        }
                        i.print("\n})\n");
                        break;
                    }
                    case UNTIL_LOOP_END: {
                        i.print("\nuntil " + i.repeatloopcondition + "\n");
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
            case PERCENTUAL: PRINT("%")
            case CARET: PRINT("^")
            case HASHTAG: PRINT("#")
            case METHOD: PRINT(":")
            case TWODOTS: PRINT("..")
            case TREDOTS: PRINT("...")
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
            case CONCATENATE: {
                i.parseVariable();
                PRINT(" = " + i.lastvariable + " .. ")
            }
            case EQUAL: case BIGGER_EQUAL: case SMALLER_EQUAL: case BIGGER: {
                i.parseComparison(t.lexeme, t.lexeme);
                break;
            }
            case NOT_EQUAL: {
                i.parseComparison("!=", "~=");
                break;
            }
            case SMALLER: {
                token check = i.peek();
                if (check.type == IDENTIFIER && i.peek(1).type == BIGGER) {
                    if (i.labels.find(check.lexeme) != i.labels.end()) i.error("Label '" + check.lexeme + "' already exists");
                    i.labels.insert(check.lexeme);
                    i.print("::" + check.lexeme + "::\n");
                    i.advance();
                    i.advance();
                    break;
                }
                i.parseComparison("<", "<");
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
                if (i.queue.top() != FOR_LOOP || array.type != IDENTIFIER) i.unexpected("in");
                i.print(" in ipairs(" + array.lexeme + ")");
                i.advance();
                break;
            }
            case WITH: {
                if (i.queue.top() != FOR_LOOP) i.unexpected("with");
                i.print(" in ");
                break;
            }
            case WHILE: CODEBLOCK("while ", WHILE_LOOP)
            case NEW: {
                i.expect(i.peek(), {CURLY_BRACKET_OPEN});
                i.advance();
                bool meta = false;
                uint cscope = 0;
                for (uint it = i.current; i.tokens[it].type != CURLY_BRACKET_CLOSED && cscope == 0; it++) {
                    tokentype type = i.tokens[it].type;
                    printf("%d %d %d %d\n", cscope, type, IDENTIFIER);
                    //printf("%s %d\n", i.tokens[it].literal, cscope);
                    switch (type) {
                        case EOF: {
                            i.expectedBeforeEOF("}");
                            break;
                        }
                        case CURLY_BRACKET_OPEN: {
                            cscope++;
                            printf("%d\n", cscope);
                            break;
                        }
                        case CURLY_BRACKET_CLOSED: {
                            cscope--;
                            break;
                        }
                        case META: {
                            meta = true;
                            break;
                        }
                    }
                }
                if (meta) {
                    i.metatables.push(std::list<method>());
                    CODEBLOCK("setmetatable({\n", METATABLE_END)
                }
                CODEBLOCK("{\n", TABLE_END)
            }
            case META: {
                if (i.queue.top() != METATABLE_END) i.unexpected("meta");
                i.metatables.top().push_back(method(i.advance().lexeme, i));
                break;
            }
            case UNTIL: {
                std::vector<token> conditionTokens;
                for (uint it = i.current; i.tokens[it].type != CURLY_BRACKET_OPEN; it++) {
                    if (i.tokens[it].type == EOF) i.expectedBeforeEOF("{");
                    conditionTokens.push_back(i.tokens[it]);
                    i.advance();
                }
                conditionTokens.push_back(token(EOF, "", "", 0));
                i.repeatloopcondition = ParseTokens(conditionTokens, filename);
                i.queue.push(UNTIL_LOOP_START);
                break;
            }
            case GOTO: {
                std::string label = i.peek().lexeme;
                if (i.labels.find(label) == i.labels.end()) i.error("Unknown label '" + label + "'");
                PRINT("goto ")
            }
            case LOCAL: PRINT("local ")
            case RETURN: PRINT("return ")
            case THIS: PRINT("this")
            case TRUE: PRINT("true")
            case FALSE: PRINT("false")
            case NIL: PRINT("nil")
        }
    }
    if (i.pscope > 0) i.expectedBeforeEOF(")");
    if (i.qscope > 0) i.expectedBeforeEOF("]");
    return i.output;
}

#define EOF -1