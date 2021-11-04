#include <unordered_map>
#include <list>
#define ADDTOKEN(type) addtoken(code, type, "", start, current); break;

std::unordered_map<std::string, std::string> files;

typedef unsigned int uint;

enum tokentype {
	//single characters
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED,
	SQUARE_BRACKET_OPEN, SQUARE_BRACKET_CLOSED,
	CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	COMMA, DOT, COLON, SEMICOLON, PLUS, MINUS, STAR, SLASH,
	NOT, AND, OR,
	
	//definition and comparison
	DEFINE, DEFINEIF, INCREASE, DECREASE, MULTIPLY, DIVIDE,
	EQUAL, NOT_EQUAL, BIGGER, BIGGER_EQUAL, SMALLER, SMALLER_EQUAL,
	
	//literals
	NUMBER, STRING, TABLE,
	
	//keywords
	DO, IF, ELSEIF, ELSE, FOR, WHILE, UNTIL, GOTO,
	LOCAL, RETURN, THIS, TRUE, FALSE, NIL, REQUIRE,
	
	#undef EOF
	EOF = -1
	#define EOF -1
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

char readnext(std::string &code, uint &current) {
	return code.at(current++);
}

void addtoken(std::string &code, std::list<token> tokens, tokentype type, std::string literal, uint &line, uint &start, uint &current) {
	std::string text = code.substr(start, current);
	tokens.insert(token(type, text, literal, ))
}

std::list<token> scanfile(std::string code, std::string filename) {
	uint line = 1, start = 0, current = 0;
	std::list<token> tokens;
	while (current < code.length()) {
		start = current;
		char c = readnext(code, current);
		switch (c) {
			case 'c': 
		}
	}
	tokens.insert(token(EOF, "", "", line));
	return tokens;
}
