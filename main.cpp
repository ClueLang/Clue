#include <stdio.h>
#include <direct.h>
#include <iostream>
#include <string>
#include "parser.cpp"

//VERSION 4 BETA 0.1

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
		printf("%s\n", errormsg.c_str());
		system("pause");
	}
}
