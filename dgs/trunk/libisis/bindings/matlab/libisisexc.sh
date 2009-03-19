#!/bin/sh
#
# $Id: libisisexc.sh 1443 2008-11-24 11:53:39Z Freddie Akeroyd $
#
# Generate libisisexc.h from libisisexc.txt (passed as $1 argument)
#
echo "#ifndef LIBISISEXC_H" > libisisexc.h
echo "#define LIBISISEXC_H" >> libisisexc.h
echo "/*" >> libisisexc.h
echo " * !!! DO NOT EDIT !!!" >> libisisexc.h
echo " * !!! Automatically generated from libisisexc.txt by $Id: libisisexc.sh 1443 2008-11-24 11:53:39Z Freddie Akeroyd $ !!!" >> libisisexc.h
echo " */" >> libisisexc.h

( while read class op nlhs nrhs junk; do 
  if test "$class" != "" -a "${class:0:1}" != "#"; then 
    lfunc=`echo ixb${op}_${class} | tr 'A-Z' 'a-z' | sed -e 's/ixt//'`
    ufunc=`echo IXB${op}_${class} | tr 'a-z' 'A-Z' | sed -e 's/IXT//'`
#    echo "#define $lfunc $ufunc" >> libisisexc.h 
##    echo "#ifdef __APPLE__" >> libisisexc.h
##    echo "#    define $lfunc ${lfunc}_" >> libisisexc.h
##    echo "#else" >> libisisexc.h
    echo "#    define $lfunc ${lfunc}__" >> libisisexc.h
##    echo "#endif /* __APPLE__ */" >> libisisexc.h
#    if expr index $lfunc _; then
#        echo "#define $lfunc ${lfunc}__" >> libisisexc.h
#    else
#        echo "#define $lfunc ${lfunc}_" >> libisisexc.h
#    fi
    echo "declare_function($lfunc)" >> libisisexc.h 
  fi
  done ) < $1

echo "static mexfunc_s_t mex_functions[] = {" >> libisisexc.h
( while read class op nlhs nrhs junk; do
  if test "$class" != "" -a "${class:0:1}" != "#"; then 
    lfunc=`echo ixb${op}_${class} | tr 'A-Z' 'a-z' | sed -e 's/ixt//'`
    echo "{ \"${class}_${op}\", $lfunc }," >> libisisexc.h
  fi 
  done ) < $1
echo "{ NULL, NULL }" >> libisisexc.h
echo "};" >> libisisexc.h

echo "#endif /* LIBISISEXC_H */" >> libisisexc.h

