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


#Name of file that contain all the files to take into account
$config_file = "wc_config.txt"; 
$output_file = "wc_output.txt";



@input_arg = @ARGV;
if ($#input_arg == -1) {
  display_manual();
  exit;
}

if ($input_arg[0] eq '-c' || $input_arg[0] eq '--config') {
  create_config_file();
}
elsif ($input_arg[0] eq '-h' || $input_arg[0] eq '--help') {
  display_manual();
} else {
  count_line();
}


sub create_config_file {

}




sub   display_manual {
  print "\nwc_idl [list_of_folders_to_check>/-c/-h]\n\n";
  print "      -c, --config    will cretae the wc_config.txt file used by\n";
  print "      -h, --help      will display this message";
  print "wc_idl\n\n";
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


