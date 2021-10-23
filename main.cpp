#include <stdio.h>
#include <dirent.h>
#include <iostream>
#include <fstream>
#include <stack>
#include <string>

std::stack<unsigned int> stack;

int main(int argc, char** argv) {
	/*std::ifstream code;
	code.open("main.cpp", std::ifstream::in);
	std::string test;
	while (code >> test) {
		printf("%s\n", test.c_str());
	}*/
	std::string codepath, endpath;
	if (argc >= 2)
		codepath = argv[1];
	else {
		printf("Please insert path to code directory: ");
		std::getline(std::cin >> std::ws, codepath);
	}
	if (argc == 3)
		codepath = argv[2];
	else {
		printf("Please insert path to output directory: ");
		std::getline(std::cin >> std::ws, endpath);
	}
}
