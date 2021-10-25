#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <unordered_map>
#include <stack>
#include <vector>
#define ERROR(msg) throw std::string("Error in file \"" + filename + "\" at line [" + std::to_string(line) + "]: " + msg)
#define SPACEIF(msg) !spaced ? std::string(std::string(" ") + msg).c_str() : msg
#define PARSETOKEN \
	prevcompiled = compiling; \
	switch (str2hash(prevcompiled.c_str())) { \
		case str2hash("if"): { \
			scope.push(token(line, TOKEN_IF)); \
			fprintf(output, "%s", "if"); \
			break; \
		} \
		case str2hash("while"): { \
			scope.push(token(line, TOKEN_WHILE)); \
			fprintf(output, "%s", "while"); \
			break; \
		} \
		default: { \
			fprintf(output, "%s", prevcompiled.c_str()); \
			break; \
		} \
	} \
	compiling.clear();


std::string codepath;
std::unordered_map<std::string, std::string> files;

enum token_type {
	TOKEN_DO,
	TOKEN_IF,
	TOKEN_WHILE,
	TOKEN_UNTIL,
};

struct token {
	unsigned int line;
	token_type type;
	token(unsigned int line, token_type type) :
    	line(line),
    	type(type)
	{}
};

constexpr unsigned int str2hash(const char *str, int h = 0) {
	return !str[h] ? 5381 : (str2hash(str, h + 1) * 33) ^ str[h];
}

void compilefile(std::string path, std::string filename) {
	bool spaced = false;
	unsigned int pscope = 0, line = 1;
	std::string prevcompiled, compiling;
	std::stack<token> scope, inscope;
	
	std::string compiledname = filename == "main.clue" ? "main.lua" : std::string(std::to_string(files.size()) + ".lua");
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
				continue;
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
				if (scope.empty()) ERROR("Unexpected token \"{\".");
				token poptoken = scope.top();
				scope.pop();
				inscope.push(poptoken);
				switch (poptoken.type) {
					case TOKEN_IF: {
						fprintf(output, "%s", SPACEIF("then"));
						break;
					}
					case TOKEN_WHILE: {
						fprintf(output, "%s", SPACEIF("do"));
						break;
					}
					default: {
						ERROR("Unknown token in stack.");
						break;
					}
				}
				spaced = false;
				continue;
			}
			case '}': {
				PARSETOKEN
				if (inscope.empty()) ERROR("Unexpected token \"}\".");
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
		}
		spaced = false;
		if (crt) compiling += crt;
	}
	fprintf(output, "%s", compiling.c_str());
	fclose(output);
	fclose(code);
}

void compilefolder(std::string path) {
	DIR *directory = opendir(path.c_str());
	if (directory == NULL) {
		std::string errormsg = "Failed to open directory \"";
		errormsg += path + "\"";
		throw errormsg;
	}
	struct dirent *entry;
	while (entry = readdir(directory)) {
		std::string name = entry->d_name;
		if (name != "." && name != "..") {
			struct stat s;
			stat((path + "\\" + name).c_str(), &s);
			if (s.st_mode & S_IFDIR) {
				compilefolder(path + "\\" + name);
			} else if (name.find(".clue") != name.npos) {
				compilefile(path + "\\" + name, name);
			}
		}
	}
	closedir(directory);
}
