#include <unordered_map>
#undef EOF
#define ADDTOKEN(type) i.addToken(type, ""); break;

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
	LOCAL, RETURN, THIS, TRUE, FALSE, NIL,
	
	EOF = -1
};

const std::unordered_map<std::string, tokentype> keyWords = {
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
	uint line = 1, start = 0, current = 0;
	std::string code, filename;
	std::vector<token> tokens = std::vector<token>();
	
	codeinfo(std::string code, std::string filename) :
		code(code),
		filename(filename)
	{}
	
	bool ended() {
		return current >= code.length();
	}
	
	char readNext() {
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
	
	bool isNumber(char c) {
		return c >= '0' && c <= '9';
	}
	
	bool isChar(char c) {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
	}
	
	bool isCharOrNumber(char c) {
		return isChar(c) || isNumber(c);
	}
	
	std::string substr(uint start, uint end) {
		std::string result;
		for (int i = start; i < end; i++) result += code.at(i);
		return result;
	}

	void addToken(tokentype type, std::string literal) {
		std::string text = substr(start, current);
		tokens.push_back(token(type, text, literal, line));
	}
	
	void warning(std::string message) {
		printf("%s\n", std::string("Error in file \"" + filename + "\" at line [" + std::to_string(line) + "]: " + message).c_str());
		errored = true;
	}
};

std::vector<token> ScanFile(std::string code, std::string filename) {
	codeinfo i(code, filename);
	while (!i.ended()) {
		i.start = i.current;
		char c = i.readNext();
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
					while (i.peek() != '\n' && !i.ended()) i.readNext();
				} else if (i.compare('*')) {
					while (!i.ended() && !(i.peek() == '*' && i.peek(1) == '/')) {
						if (i.peek() == '\n') i.line++;
						i.readNext();
					}
					if (i.ended()) {
						i.warning("Unterminated comment.");
					}
				} else {
					ADDTOKEN(SLASH)
				}
				break;
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
					i.readNext();
				}
				if (i.ended()) {
					i.warning("Unterminated string.");
					break;
				}
				i.readNext();
				std::string value = i.substr(i.start + 1, i.current - 1);
				i.addToken(STRING, value);
				break;
			}
			default: {
				if (i.isNumber(c)) {
					while (i.isNumber(i.peek())) i.readNext();
						if (i.peek() == '.' && i.isNumber(i.peek(1))) {
						i.readNext();
						while (i.isNumber(i.peek())) i.readNext();
					}
					i.addToken(NUMBER, i.substr(i.start, i.current));
				} else if (i.isChar((c))) {
					while (i.isCharOrNumber(i.peek())) i.readNext();
					std::string text = i.substr(i.start, i.current);
					tokentype type = IDENTIFIER;
					if (keyWords.find(text) != keyWords.end()) type = keyWords.at(text);
					i.addToken(type, "");
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
