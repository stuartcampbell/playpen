rem
rem $Id: libisisexc_2.bat 294 2004-08-18 12:55:50Z jdmc43 $
rem
rem called with args class method e.g. IXTtestclass plus
rem

set lfunc=%2_%1
set lfunc=%lfunc:A=a%
set lfunc=%lfunc:B=b%
set lfunc=%lfunc:C=c%
set lfunc=%lfunc:D=d%
set lfunc=%lfunc:E=e%
set lfunc=%lfunc:F=f%
set lfunc=%lfunc:G=g%
set lfunc=%lfunc:H=h%
set lfunc=%lfunc:I=i%
set lfunc=%lfunc:J=j%
set lfunc=%lfunc:K=k%
set lfunc=%lfunc:L=l%
set lfunc=%lfunc:M=m%
set lfunc=%lfunc:N=n%
set lfunc=%lfunc:O=o%
set lfunc=%lfunc:P=p%
set lfunc=%lfunc:Q=q%
set lfunc=%lfunc:R=r%
set lfunc=%lfunc:S=s%
set lfunc=%lfunc:T=t%
set lfunc=%lfunc:U=u%
set lfunc=%lfunc:V=v%
set lfunc=%lfunc:W=w%
set lfunc=%lfunc:X=x%
set lfunc=%lfunc:Y=y%
set lfunc=%lfunc:Z=z%

set lfunc=%lfunc:ixt=%

echo { "%1_%2", ixb%lfunc% }, >> libisisexc.h

