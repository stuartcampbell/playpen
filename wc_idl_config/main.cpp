/*
 * main.cpp
 *
 * This program will create a config file that will be used by the main
 * part of wc_idl to get the number of lines of code
 *
 * this program will work this way:
 * ex:
 *       wc_idl_config  <list_of_files>|*  -o <name of config file>
 *
 * [config file]
 * 		ref_reduction.pro
 * 	    ref_reduction
 *      REFreduction.cfg
 *      REF_L_3653.nxs
 *      makefile
 *      REFreduction_images/up.png
 *      REFreduction_images/down.png
 *      REFreduction_images/left.png
 *      .REFreduction.cfg
 *
 *  Created on: Jan 20, 2010
 *      Author: j35
 */

#include <iostream>
#include <string>
using namespace std;

void display_help_message (); //display instructions on how the program works

int main(int argc, char * argv[]) {

	//decode arguments
	if (argc < 2) {
		display_help_message();
		exit(0);
	}

	string str; //will contain the current argument tested

	//get name of output file
	for (int i = 1; i < argc; i++) {
		str = argv[i];

		if (str.compare("-o") == 0) {
			if (i+1 == argc) {
				display_help_message();
				exit(0);
			}
			cout << "Name of config file is: " << argv[i+1] << endl;
		}
	}

	return 0;

}

//show how the program works (the way to edit the command line)
void display_help_message () {
	cout << "wc_idl_config <list_of_files>|* -o "
			"<name_of_output_config_file>" << endl;
}
