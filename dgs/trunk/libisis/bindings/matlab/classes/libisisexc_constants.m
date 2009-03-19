% Matlab script that sets up global constants that match the values in LIBISISEXC

% There is probably a better way to do this; the script is a temporary fix.

% shapes:
global IXCpoint IXCbox IXCcylinder IXCsphere IXCholcyl IXCpolygon

IXCpoint=0; IXCbox=1; IXCcylinder=2; IXCsphere=3; IXCholcyl=4; IXCpolygon=5;

% projections:
global  IXCspherical_polar IXCcylindrical_polar IXCpolar IXCplanar

IXCspherical_polar=1; IXCcylindrical_polar=2; IXCpolar=3; IXCplanar=4;