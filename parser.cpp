#include <sys/stat.h>
#include <dirent.h>
#include <string.h>
#include <unordered_map>
#include <stack>
#include <vector>
#define ERROR(msg) throw std::string("Error in file \"" + filename + "\" at line [" + std::to_string(line) + "]: " + msg)

std::string codepath;
std::unordered_map<std::string, std::string> files;

constexpr unsigned int str2hash(const char *str, int h = 0) {
	return !str[h] ? 5381 : (str2hash(str, h + 1) * 33) ^ str[h];
}

void compilefile(std::string path, std::string filename) {
	unsigned int pscope = 0, line = 1;
	std::string prevcompiled, compiling;
	std::stack<unsigned int> scope;
	
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
				continue;
			}
			case '\n': {
				line++;
				fprintf(output, "\n");
				continue;
			}
			case '(': {
				pscope++;
				break;
			}
			case ')': {
				if (pscope <= 0) ERROR("Unexpected token \")\" found.");
				pscope--;
				break;
			}
			case '{': {
				//check stack
				continue;
			}
			case '}': {
				//check stack
				continue;
			}
			case ' ': {
				prevcompiled = compiling;
				compiling.clear();
				//examine the token
				switch (str2hash(prevcompiled.c_str())) {
					default: {
						fprintf(output, "%s ", prevcompiled.c_str());
						break;
					}
				}
				continue;
			}
		}
		compiling += crt;
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
