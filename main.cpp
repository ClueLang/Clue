#include <stdio.h>
#include <direct.h>
#include <sys/stat.h>
#include <dirent.h>
#include <iostream>
#include <string>
#include "scanner.cpp"
#include "treemaker.cpp"

//VERSION 7 BETA 2.0

std::string codepath;

void compilefile(std::string path, std::string filename) {
	std::string compiledname = filename == "main.clue" ? "main.lua" : filename == "init.clue" ? "init.lua" : std::string(std::to_string(files.size()) + ".lua");
	files[filename] = compiledname;
	FILE *output = fopen((codepath + "\\.clue\\" + compiledname).c_str(), "w+");
	if (output == NULL) {
		std::string errormsg = "Failed to create output file \"";
		errormsg += codepath + "\\.clue\\" + compiledname + "\"";
		throw errormsg;
	}
	FILE *codefile = fopen(path.c_str(), "rb");
	if (codefile == NULL) {
		std::string errormsg = "Failed to open code file \"";
		errormsg += path + "\"";
		throw errormsg;
	}
	std::string code;
	char c;
	while ((c = fgetc(codefile)) != EOF) {
		code += c;
	}
	fclose(codefile);
	std::list<token> tokens = scanfile(code, filename);
	fclose(output);
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

int main(int argc, char** argv) {
	/*std::ifstream code;
	code.open("main.cpp", std::ifstream::in);
	std::string test;
	while (code >> test) {
		printf("%s\n", test.c_str());
	}*/
	if (argc >= 2)
		codepath = argv[1];
	else {
		printf("Please insert path to code directory: ");
		std::getline(std::cin >> std::ws, codepath);
	}
	_mkdir((codepath + "\\.clue").c_str());
	try {
		compilefolder(codepath);
	} catch (std::string errormsg) {
		printf("%s\nCannot proceed, compilation stopped.\n", errormsg.c_str());
		system("pause");
	}
}
