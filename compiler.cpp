struct tokensinfo {
    uint line = 1, start = 0, current = 0;
	std::string filename;
	std::vector<token> tokens;
    FILE *output;
	
	tokensinfo(std::vector<token> tokens, std::string filename, FILE *output) :
		tokens(tokens),
		filename(filename),
        output(output)
	{}

    bool ended() {
        return tokens[current].type == EOF;
    }

    token advance() {
        return tokens[current++];
    }
};

void ParseTokens(std::vector<token> tokens, std::string filename, FILE *output) {
    tokensinfo i(tokens, filename, output);
    while(!i.ended()) {
        i.start = i.current;
        token t = i.advance();
        switch (t.type) {
            //case IDENTIFIER: printf("%s %s\n", t.lexeme.c_str(), t.literal.c_str());
        }
    }
}
