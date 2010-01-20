/*
 * check_input.cpp
 *
 *  Created on: Jan 20, 2010
 *      Author: j35
 */

#include "check_input.h"
#include <string>
#include <iostream>
using namespace std;

void check_input_from_user(int argc, char * argv[], int & config_file_index) {

	//decode arguments
	if (argc < 2) {
		display_help_message();
		exit(0);
	}

	string str; //will contain the current argument tested

	//get name of output file
	for (int i = 1; i < argc; i++) {
		str = argv[i];

//		cout << "argv[" << i << "]= " << argv[i] << endl;

		if (str.compare("-o") == 0) {
			if (i + 1 == argc) {
				display_help_message();
				exit(0);
			}
//			cout << "Name of config file is: " << argv[i + 1] << endl;
			config_file_index = i+1;
		}

	}

}

//show how the program works (the way to edit the command line)
void display_help_message() {
	cout << "wc_idl_config <list_of_files>|* -o "
		"<name_of_output_config_file>" << endl;
}
