#include <stack>

struct tokensinfo {
    uint line = 1, current = 0;
	std::string filename;
	std::vector<token> tokens;
    std::stack<tokentype> queue();
    FILE *output;
	
	tokensinfo(std::vector<token> tokens, std::string filename, FILE *output) :
		tokens(tokens),
		filename(filename),
        output(output)
	{}

    void print(std::string text) {
        fprintf(output, "%s", text.c_str());
    }

    void error(std::string message) {
        fclose(output);
        throw message;
    }

    bool ended() {
        return tokens[current + 1].type == EOF;
    }

    token advance() {
        return tokens[current++];
    }

    token peek() {
        return tokens[current + 1];
    }

    void parse() {
        token t = i.advance();
        if (t.type == EOF) error("Unexpected token <eof>.");
        switch (t.type) {
            //case IDENTIFIER: printf("%s %s\n", t.lexeme.c_str(), t.literal.c_str());
            case CURLY_BRACKET_OPEN: {
                switch (queue) {

                }
            }
            case IF: {
                i.print("if ");
                queue.push(IF);
                parse();
                break;
            }
        }
    }
};

void ParseTokens(std::vector<token> tokens, std::string filename, FILE *output) {
    tokensinfo i(tokens, filename, output);
    while(!i.ended()) {
        i.parse();
    }
}
