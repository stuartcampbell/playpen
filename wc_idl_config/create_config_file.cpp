/*
 * create_config_file.cpp
 *
 *  Created on: Jan 20, 2010
 *      Author: j35
 */

#include <string>
#include <fstream>
#include <iostream>
#include "create_config_file.h"
using namespace std;

void create_config_file_name(std::string config_file_name, int config_index,
		int argc, char *argv[]) {

	char * config_file;
	config_file = new char[config_file_name.length()+1];
	strcpy(config_file,config_file_name.c_str());

	fstream output_file(config_file,ios::out|ios::app);
	if (output_file.is_open()) {

		output_file.close();
	}
	else {
		cout << output_file << " can not be opened correctly!" << endl;
	}


}
