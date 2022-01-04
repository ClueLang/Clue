#![allow(non_camel_case_types)]

enum TokenType {
	//symbols
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED,
	SQUARE_BRACKET_OPEN, SQUARE_BRACKET_CLOSED,
	CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	COMMA, DOT, SEMICOLON, NOT, AND, OR, DOLLAR,
	PLUS, MINUS, STAR, SLASH, PERCENTUAL, CARET,
	HASHTAG, METHOD, TWODOTS, TREDOTS,
	
	//definition and comparison
	DEFINE, DEFINEIF, INCREASE, DECREASE, MULTIPLY, DIVIDE, EXPONENTIATE, CONCATENATE,
	EQUAL, NOT_EQUAL, BIGGER, BIGGER_EQUAL, SMALLER, SMALLER_EQUAL, LAMBDA,
	
	//literals
	IDENTIFIER, NUMBER, STRING,
	
	//keywords
	DO, IF, ELSEIF, ELSE, FOR, OF, IN, WITH, WHILE, NEW, META,
	UNTIL, GOTO, LOCAL, RETURN, THIS, TRUE, FALSE, NIL,
	
	EOF = -1
}

struct Token {
    kind: TokenType,
    lexeme: String,
    literal: String,
    line: u32
}

impl Token {
    fn new(kind: TokenType, lexeme: String, literal: String, line: u32) -> Token {
        Token {
            kind: kind,
            lexeme: String::from(lexeme),
            literal: String::from(literal),
            line: line
        }
    }
}

struct CodeInfo {
    line: u32,
    start: usize,
    current: usize,
    size: usize,
    code: String,
    filename: String,
    tokens: Vec<Token>,
    errored: bool,
}

impl CodeInfo {
    fn new(code: String, filename: String) -> CodeInfo {
        CodeInfo {
            line: 1,
            start: 0,
            current: 0,
            size: code.chars().count(),
            code: code,
            filename: filename,
            tokens: Vec::new(),
            errored: false
        }
    }

    fn ended(&self) -> bool {
        self.current >= self.size
    }

    fn at(&self, pos: usize) -> char {
        self.code.as_bytes()[pos] as char
    }

    fn readNext(&mut self) -> char {
        self.current = self.current + 1;
        self.at(self.current)
    }

    fn compare(&mut self, expected: char) -> bool {
        if self.ended() {return false;}
        if self.at(self.current) != expected {return false;}
        self.current = self.current + 1;
        true
    }

    fn peek(&self, pos: usize) -> char {
        let pos: usize = self.current + pos;
        if pos >= self.size {return 0 as char;}
        self.at(pos)
    }

    //isNumber: c.is_ascii_digit()
    //isChar: c.is_ascii_alphabetic()
    //isCharOrNumber: c.is_ascii_alphanumeric()

    fn substr(&self, start: usize, end: usize) -> String {
        let mut result: String = String::new();
        for i in start..end {
            result.push(self.at(i));
        }
        result
    }

    fn addLiteralToken(&mut self, kind: TokenType, literal: String) {
        let lexeme: String = self.substr(self.start, self.current);
        self.tokens.push(Token::new(kind, lexeme, literal, self.line))
    }

    fn addToken(&mut self, kind: TokenType) {
        self.addLiteralToken(kind, String::new())
    }

    fn warning(&self, message: String) {
        println!("Error in file \"{}\" at line [{}]!\nError: \"{}\"", self.filename, self.line, message);
    }
}

pub fn ScanFile() -> Vec<Token> {
    
}