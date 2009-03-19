function user = IXTuser( varargin )
% ! Create an IXTuser object
% ! REQUIRED INPUT PARAMETERS
% ! user  = IXTuser( base, 'name', 'affiliation', 'address',
%                        'telephone_number', 'fax_number', 'email');
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! ISISEXCuser
% ! ===========
% ! NeXus class: NXuser
% !
% !	base			IXTbase
% !	name				char	user name
% !	affiliation			char
% !	address				char	postal address
% !	telephone_number	char
% !	fax_number			char
% !	email				char
% !
% !


    % if no input arguments, create a default object
    user.base =  IXTbase;
    user.name = 'name';
    user.affiliation = 'affil';
    user.address = 'address';
    user.telephone = 'telephone'; 
    user.fax = 'fax';    
    user.email = 'email';
    
    user = class(user,'IXTuser');
if (nargin > 0)
    user = libisisexc('IXTuser','create',user,varargin);
end    
