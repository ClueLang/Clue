#include <unordered_map>
#include <vector>
#undef EOF
#define ADDTOKEN(type) i.addtoken(type, ""); break;

std::unordered_map<std::string, std::string> files;
bool errored = false;

typedef unsigned int uint;

enum tokentype {
	//single characters
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED,
	SQUARE_BRACKET_OPEN, SQUARE_BRACKET_CLOSED,
	CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	COMMA, DOT, SEMICOLON, PLUS, MINUS, STAR, SLASH,
	NOT, AND, OR, DOLLAR,
	
	//definition and comparison
	DEFINE, DEFINEIF, INCREASE, DECREASE, MULTIPLY, DIVIDE,
	EQUAL, NOT_EQUAL, BIGGER, BIGGER_EQUAL, SMALLER, SMALLER_EQUAL,
	
	//literals
	IDENTIFIER, NUMBER, STRING,
	
	//keywords
	DO, IF, ELSEIF, ELSE, FOR, WHILE, UNTIL, GOTO,
	LOCAL, RETURN, THIS, TRUE, FALSE, NIL, REQUIRE,
	
	EOF = -1
};

const std::unordered_map<std::string, tokentype> keywords = {
	{"do", DO},
	{"if", IF},
	{"elseif", ELSEIF},
	{"else", ELSE},
	{"for", FOR},
	{"while", WHILE},
	{"until", UNTIL},
	{"goto", GOTO},
	{"local", LOCAL},
	{"return", RETURN},
	{"this", THIS},
	{"true", TRUE},
	{"false", FALSE},
	{"nil", NIL},
	{"require", REQUIRE}
};

struct token {
	tokentype type;
	std::string lexeme;
	std::string literal;
	uint line;
	
	token(tokentype type, std::string lexeme, std::string literal, uint line) :
		type(type),
		lexeme(lexeme),
		literal(literal),
		line(line)
	{}
};

struct codeinfo {
	uint line = 1;
	uint start = 0;
	uint current = 0;
	std::string code, filename;
	std::vector<token> tokens = std::vector<token>();
	
	codeinfo(std::string code, std::string filename) :
		code(code),
		filename(filename)
	{}
	
	bool ended() {
		return current >= code.length();
	}
	
	char readnext() {
		return code.at(current++);
	}
	
	bool compare(char expected) {
		if (ended()) return false;
		if (code.at(current) != expected) return false;
		current++;
		return true;
	}
	
	char peek(short pos = 0) {
		return current + pos >= code.length() ? '\0' : code.at(current + pos);
	}
	
	bool isnumber(char c) {
		return c >= '0' && c <= '9';
	}
	
	bool ischar(char c) {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
	}
	
	bool ischarornumber(char c) {
		return ischar(c) || isnumber(c);
	}
	
	void addtoken(tokentype type, std::string literal) {
		std::string text = code.substr(start, current);
		tokens.push_back(token(type, text, literal, line));
	}
	
	void warning(std::string message) {
		printf("%s\n", std::string("Error in file \"" + filename + "\" at line [" + std::to_string(line) + "]: " + message).c_str());
		errored = true;
	}
};

std::vector<token> scanfile(std::string code, std::string filename) {
	codeinfo i(code, filename);
	while (!i.ended()) {
		i.start = i.current;
		char c = i.readnext();
		switch (c) {
			case '(': ADDTOKEN(ROUND_BRACKET_OPEN)
			case ')': ADDTOKEN(ROUND_BRACKET_CLOSED)
			case '[': ADDTOKEN(SQUARE_BRACKET_OPEN)
			case ']': ADDTOKEN(SQUARE_BRACKET_CLOSED)
			case '{': ADDTOKEN(CURLY_BRACKET_OPEN)
			case '}': ADDTOKEN(CURLY_BRACKET_CLOSED)
			case ',': ADDTOKEN(COMMA)
			case '.': ADDTOKEN(DOT)
			case ';': ADDTOKEN(SEMICOLON)
			case '+': ADDTOKEN(i.compare('=') ? INCREASE : PLUS)
			case '-': ADDTOKEN(i.compare('=') ? DECREASE : MINUS)
			case '*': ADDTOKEN(i.compare('=') ? MULTIPLY : STAR)
			case '/': {
				if (i.compare('/')) {
					while (i.peek() != '\n' && !i.ended()) i.readnext();
					break;
				} else if (i.compare('*')) {
					while (!(i.peek() == '*' && i.peek(1) == '/')) {
						if (i.peek() == '\n') i.line++;
						i.readnext();
					}
				} else {
					ADDTOKEN(SLASH)
				}
			}
			case '!': ADDTOKEN(i.compare('=') ? NOT_EQUAL : NOT)
			case '=': ADDTOKEN(i.compare('=') ? EQUAL : DEFINE)
			case '<': ADDTOKEN(i.compare('=') ? SMALLER_EQUAL : SMALLER)
			case '>': ADDTOKEN(i.compare('=') ? BIGGER_EQUAL : BIGGER)
			case '?': case '&': ADDTOKEN(AND)
			case ':': case '|': ADDTOKEN(OR)
			case '$': ADDTOKEN(DOLLAR)
			case ' ': case '\r': case '\t': break;
			case '\n': i.line++; break;
			case '"': {
				while (!i.ended() && i.peek() != '"') {
					if (i.peek() == '\n') i.line++;
					i.readnext();
				}
				if (i.ended()) {
					i.warning("Unterminated string.");
					break;
				}
				i.readnext();
				std::string value = i.code.substr(i.start + 1, i.current - 1);
				i.addtoken(STRING, value);
				break;
			}
			default: {
				if (i.isnumber(c)) {
					while (i.isnumber(i.peek())) i.readnext();
						if (i.peek() == '.' && i.isnumber(i.peek(1))) {
						i.readnext();
						while (i.isnumber(i.peek())) i.readnext();
					}
					i.addtoken(NUMBER, i.code.substr(i.start, i.current));
				} else if (i.ischar((c))) {
					while (i.ischarornumber(i.peek())) i.readnext();
					std::string text = i.code.substr(i.start, i.current);
					tokentype type = IDENTIFIER;
					if (keywords.find(text) != keywords.end()) type = keywords.at(text);
					i.addtoken(type, "");
				} else {
					std::string errormsg = "Unexpected character '";
					errormsg += c;
					errormsg += "'.";
					i.warning(errormsg);
				}
				break;
			}
		}
	}
	i.tokens.push_back(token(EOF, "", "", i.line));
	return i.tokens;
}

#define EOF -1
