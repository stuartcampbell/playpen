/********************************************************************* 
 ffind.c 	Usage fpos=ffind(filename,string)

 This mex file is intended to help matlab deal with large data files.
 It returns a pointer to the beginning of the first line containing
 the string 'string'. THe matlab function fseek can then be used.
 
 M. Zinkin 9.5.94
**********************************************************************/

#include <stdlib.h>
#include <stdio.h> 
#include <string.h> 
#include "mex.h"
                                                                  
#define MAXLINE 200    
#define READFILE 	prhs[0]
#define STRING		prhs[1] 
#define OUT		plhs[0]

/* version 5 -> 4 modifs :  mxArray -> Matrix, mxREAL -> 0 */

void   mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[]);
                    
void   mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{                                          
   FILE *ReadPtr;
   char ReadFile[MAXLINE+1], String[MAXLINE+1], Line[MAXLINE+1];        
   long fpos;

   if (nrhs!=2) mexErrMsgTxt("Syntax: <filepos>=ffind(<filename>,<search string>)");

	mxGetString(READFILE,ReadFile,MAXLINE);
	mxGetString(STRING,String,MAXLINE);

	/* Open file to read */
	if ((ReadPtr=fopen(ReadFile,"r"))==NULL) mexErrMsgTxt("Data file not found.");               
	
	/* Read through file until find Start */
	do fpos=ftell(ReadPtr);
	while ((fgets(Line,MAXLINE,ReadPtr)!=NULL) && (strstr(Line,String)==NULL)); 
	   
	if (strstr(Line,String)==NULL) fpos=-1;

	/* Return position in file */
	OUT=mxCreateDoubleMatrix(1,1,0);
	*mxGetPr(OUT)=(double) fpos;
		    
	fclose(ReadPtr);
}
