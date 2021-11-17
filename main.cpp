#include <stdio.h>
#include <direct.h>
#include <sys/stat.h>
#include <dirent.h>
#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include "scanner.cpp"
#include "compiler.cpp"

//VERSION 13 BETA 1.0

std::string codepath;

void CompileFile(std::string path, std::string filename) {
	std::string compiledname = path, outputpath = codepath + "\\.clue";
	compiledname.erase(0, codepath.length() + 1);
	{
		std::stringstream dirfinder(compiledname);
		std::string segment, dirpath = outputpath;
		while (std::getline(dirfinder, segment, '\\')) {
			if (segment.find(".clue") != segment.npos) break;
			dirpath += "\\" + segment;
			_mkdir(dirpath.c_str());
		}
	}
	compiledname = compiledname.substr(0, compiledname.length() - 5) + ".lua";
	FILE *output = fopen((outputpath + "\\" + compiledname).c_str(), "w+");
	if (output == NULL) {
		std::string errormsg = "Failed to create output file \"";
		errormsg += outputpath + "\\" + compiledname + "\"";
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
	std::vector<token> tokens = ScanFile(code, filename);
	ParseTokens(tokens, filename, output);
	fclose(output);
}

void CompileFolder(std::string path) {
	DIR *directory = opendir(path.c_str());
	if (directory == NULL) {
		std::string errormsg = "Failed to open directory \"";
		errormsg += path + "\"";
		throw errormsg;
	}
	struct dirent *entry;
	while (entry = readdir(directory)) {
		std::string name = entry->d_name;
		if (name != "." && name != ".." && name != ".clue") {
			struct stat s;
			stat((path + "\\" + name).c_str(), &s);
			if (s.st_mode & S_IFDIR) {
				CompileFolder(path + "\\" + name);
			} else if (name.find(".clue") != name.npos) {
				CompileFile(path + "\\" + name, name);
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
		CompileFolder(codepath);
	} catch (std::string errormsg) {
		printf("%s\nCannot proceed, compilation stopped.\n", errormsg.c_str());
		system("pause");
	}
}
