rem
rem $Id: libisisexc_1.bat 294 2004-08-18 12:55:50Z jdmc43 $
rem
rem called with args: class method    e.g. "IXTtestclass" "plus"
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

set ufunc=%2_%1
set ufunc=%ufunc:a=A%
set ufunc=%ufunc:b=B%
set ufunc=%ufunc:c=C%
set ufunc=%ufunc:d=D%
set ufunc=%ufunc:e=E%
set ufunc=%ufunc:f=F%
set ufunc=%ufunc:g=G%
set ufunc=%ufunc:h=H%
set ufunc=%ufunc:i=I%
set ufunc=%ufunc:j=J%
set ufunc=%ufunc:k=K%
set ufunc=%ufunc:l=L%
set ufunc=%ufunc:m=M%
set ufunc=%ufunc:n=N%
set ufunc=%ufunc:o=O%
set ufunc=%ufunc:p=P%
set ufunc=%ufunc:q=Q%
set ufunc=%ufunc:r=R%
set ufunc=%ufunc:s=S%
set ufunc=%ufunc:t=T%
set ufunc=%ufunc:u=U%
set ufunc=%ufunc:v=V%
set ufunc=%ufunc:w=W%
set ufunc=%ufunc:x=X%
set ufunc=%ufunc:y=Y%
set ufunc=%ufunc:z=Z%

set lfunc=%lfunc:ixt=%
set ufunc=%ufunc:IXT=%

set lfunc=ixb%lfunc%
set ufunc=IXB%ufunc%

echo #define %lfunc% %ufunc% >> libisisexc.h 
echo declare_function(%lfunc%) >> libisisexc.h

