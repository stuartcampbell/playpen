#!/usr/bin/perl -w

#This program count the number of lines of IDL cold and output the result
#into an wc_output.txt file
#
#the calculation starts only after the first commented line (copyrights)
#and should display the number of lines coming from personal written file
#versus used .pro files.
#
#The output file will list in the main folder name, the individual name of the
#files found there with, for each of them number of lines of code. Then the
#total number of lines of code for the folder will be given, then the total
#number for all the folders. 
#
# author: j35@ornl.gov

use File::Find;

#Name of file that contain all the files to take into account
$config_file = "wc_config.txt"; 
$output_file = "wc_output.txt";

@input_arg = @ARGV;
if ($#input_arg == -1) {
  display_manual();
  exit;
}

if ($input_arg[0] eq '-c' || $input_arg[0] eq '--config') {
  if ($#input_arg == 0) {
    display_manual();
  } else {
    create_config_file();
  }
}
elsif ($input_arg[0] eq '-h' || $input_arg[0] eq '--help') {
  display_manual();
} else {
  count_line();
}

#------------------------------------------------------------------------------
#This subroutine creates the config file that will be used to count the number
#of lines
#The files will look like this:
#
#
# ########## FITStools ##############
# FITStools/FITStoolsFITStools/FITStools
# FITStools/FITStools.cfgFITStools/FITStools.cfg
# FITStools/MainBaseEvent.proFITStools/MainBaseEvent.pro
# FITStools/MakefileFITStools/Makefile
# #FITStools/fits_open.proFITStools/fits_open.pro
# FITStools/fits_reader.proFITStools/fits_reader.pro
# FITStools/fits_tools_tab1.proFITStools/fits_tools_tab1.pro
# FITStools/fits_tools_tab1_browser.proFITStools/fits_tools_tab1_browser.pro
#
sub create_config_file {

  shift(@input_arg); #this remove the flags -c and keep the list of folders
  @list_of_folders = ();
  for $file (@input_arg) {
    print "testing file/directory: $file --> ";
    if (-d $file) { #directory
      print " directory\n";
      push @list_of_folders, "###$file";
      find (\&wanted, $file);
    } else { #file
      print " file\n";
    }
    
  }
}


sub wanted {

  print "    --> list is $_\n";
  
}



sub   display_manual {
  print "\n   This program counts the number of idl lines coded.\n";
  print "\n   wc_idl [-c <list_of_folders>|-r|-h]\n\n";
  print "      -c, --config    creates the wc_config.txt file\n";
  print "      -r, --run       count the number of idl lines coded in wc_config.txt\n";
  print "      -h, --help      displays this message";
  print "\n\nReports any bugs to j35\@ornl.gov\n\n";
}



sub count_line {
  
  open(FILE,$config_file) or die "Can not open $config_file!\n";
  chomp(@list_of_files = (<FILE>)); #remove '\n' at the end of each line
  close FILE;
  
  open(OUTPUT_FILE,">$output_file") or die "can't create $output_file!\n";
  
  for $file (@list_of_files) {
    if ($file =~ /^#{3,}/) {
      print OUTPUT_FILE $file . "\n"; #name of folder
    } else { #name of file
      print OUTPUT_FILE $file;
      $code_ln_nbr = getNbrLines($file);
      if ($code_ln_nbr == -1) {
	print OUTPUT_FILE "$file (ERROR!)\n";
      } else {
	print OUTPUT_FILE "$file ($code_ln_nbr)\n";
      }
    }
  }
  close OUTPUT_FILE;
}




# This subroutines count the number of lines the file contain
sub getNbrLines {  
  my (@file_array) = @_;
  my $file = $file_array[0];
  open(MY_FILE,$file) or return -1;
  @line_array = <MY_FILE>;
  $nbr_lines = $#line_array+1;
  close MY_FILE;
  return $nbr_lines;
}


