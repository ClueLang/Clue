#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <unordered_map>
#include <stack>
#include <vector>
/*#define ERROR(msg) throw std::string("Error in file \"" + filename + "\" at line [" + std::to_string(line) + "]: " + msg)
#define SPACEIF(msg) !spaced ? std::string(std::string(" ") + msg).c_str() : msg
#define PARSETOKEN \
	prevcompiled = compiling; \
	switch (str2hash(prevcompiled.c_str())) { \
		case str2hash("do"): { \
			queue.alter(line, TOKEN_DO); \
			fprintf(output, "%s", "do "); \
			break; \
		} \
		case str2hash("if"): { \
			queue.alter(line, TOKEN_IF); \
			fprintf(output, "%s", "if"); \
			break; \
		} \
		case str2hash("while"): { \
			queue.alter(line, TOKEN_WHILE); \
			fprintf(output, "%s", "while"); \
			break; \
		} \
		default: { \
			fprintf(output, "%s", prevcompiled.c_str()); \
			break; \
		} \
	} \
	compiling.clear();
#define PARSEQUAL(crt) \
	printf("\"%s\"\n", compiling.c_str()); \
	if (spaced) prevcompiled = compiling; compiling.clear(); \
	fprintf(output, "= %s %c ", prevcompiled.c_str(), crt); \
	compiling += ""; \
	continue;

std::string codepath;
std::unordered_map<std::string, std::string> files;

enum token_type {
	TOKEN_NULL, 	//empty token
	TOKEN_DO,		//"do ... end" starts here
	TOKEN_IF,		//"if condition then ... end" starts here
	TOKEN_WHILE,	//"whild condition do ... end" starts here
	TOKEN_UNTIL,	//"repeat ... until condition" starts here [TODO]
	TOKEN_TABLE,	//"{ ... }" starts here
};

struct token {
	unsigned int line;
	token_type type;
	std::string arg;
	
	bool valid() {
		return this->type != TOKEN_NULL ? true : false;	
	};
	
	void clear() {
		this->line = 0;
		this->type = TOKEN_NULL;
	};
	
	void alter(unsigned int line, token_type type, std::string arg = "") {
		this->line = line;
		this->type = type;
		this->arg = arg;
	};
	
	token(unsigned int line, token_type type, std::string arg = "") :
    	line(line),
    	type(type),
    	arg(arg)
	{}
};

constexpr unsigned int str2hash(const char *str, int h = 0) {
	return !str[h] ? 5381 : (str2hash(str, h + 1) * 33) ^ str[h];
}

void compilefile(std::string path, std::string filename) {
	bool spaced = false, defining = false;
	token queue = token(0, TOKEN_NULL);
	unsigned int pscope = 0, line = 1;
	std::string prevcompiled, compiling;
	std::stack<token> inscope;
	
	std::string compiledname = filename == "main.clue" ? "main.lua" : filename == "init.clue" ? "init.lua" : std::string(std::to_string(files.size()) + ".lua");
	files[filename] = compiledname;
	FILE *output = fopen((codepath + "\\.clue\\" + compiledname).c_str(), "w+");
	if (output == NULL) {
		std::string errormsg = "Failed to create output file \"";
		errormsg += codepath + "\\.clue\\" + compiledname + "\"";
		throw errormsg;
	}
	FILE *code = fopen(path.c_str(), "rb");
	if (code == NULL) {
		std::string errormsg = "Failed to open code file \"";
		errormsg += path + "\"";
		throw errormsg;
	}
	int byte;
	while ((byte = fgetc(code)) != EOF) {
		char crt = byte;
		switch (crt) {
			case '\t': {
				fprintf(output, "\t");
				spaced = true;
				continue;
			}
			case 13: {
				spaced = true;
				continue;
			}
			case '\n': {
				PARSETOKEN
				line++;
				fprintf(output, "\n");
				spaced = true;
				if (!inscope.empty() && inscope.top().type != TOKEN_TABLE) defining = false;
				continue;
			}
			case ';': {
				defining = false;
				break;
			}
			case '(': {
				pscope++;
				break;
			}
			case ')': {
				if (pscope <= 0) ERROR("Unexpected token \")\".");
				pscope--;
				break;
			}
			case '{': {
				PARSETOKEN
				if (defining) {
					inscope.push(token(line, TOKEN_TABLE));
					fprintf(output, "{");
					spaced = false;
					continue;
				}
				if (!queue.valid()) ERROR("Unexpected token \"{\".");
				inscope.push(queue);
				switch (queue.type) {
					case TOKEN_DO: break;
					case TOKEN_IF: {
						fprintf(output, "%s ", SPACEIF("then"));
						break;
					}
					case TOKEN_WHILE: {
						fprintf(output, "%s ", SPACEIF("do"));
						break;
					}
					default: {
						ERROR("Unknown token in stack.");
						break;
					}
				}
				spaced = false;
				queue.clear();
				continue;
			}
			case '}': {
				PARSETOKEN
				if (inscope.empty()) ERROR("Unexpected token \"}\".");
				if (inscope.top().type == TOKEN_TABLE) {
					inscope.pop();
					fprintf(output, "}");
					spaced = false;
					continue;
				}
				inscope.pop();
				fprintf(output, "%s", SPACEIF("end"));
				spaced = false;
				continue;
			}
			case ' ': {
				PARSETOKEN
				fprintf(output, " ");
				spaced = true;
				continue;
			}
			case '=': {
				if (!inscope.empty() && inscope.top().type == TOKEN_TABLE) break;
				defining = true;
				switch (compiling.back()) {
					case '+': PARSEQUAL('+')
					case '-': PARSEQUAL('-')
					/*default: {
						if (!spaced) prevcompiled = compiling; compiling.clear();
						break;
					}
				}
				break;
			}
		}
		spaced = false;
		if (crt) compiling += crt;
	}
	fprintf(output, "%s", compiling.c_str());
	fclose(output);
	fclose(code);
}*/

//REFERENCE FILE FOR OLD CODE, WILL BE DELETED EVENTUALLY
