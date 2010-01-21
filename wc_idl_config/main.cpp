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
#include "check_input.h"
#include "create_config_file.h"

using namespace std;

int main(int argc, char * argv[]) {

	int config_index;
	string config_file_name; //name of config. file to be created by the prog.

	check_input_from_user(argc, argv, config_index);
	config_file_name = argv[config_index];

    //Create config file name
	create_config_file_name(config_file_name, config_index, argc, argv);




	return 0;

}

