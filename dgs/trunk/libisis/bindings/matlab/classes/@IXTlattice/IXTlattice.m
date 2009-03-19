function lattice = IXTlattice( varargin )
%ISISEXCsample Create an IXTlattice object 
% ! Create an IXTlattice object
% ! REQUIRED INPUT PARAMETERS
% ! sample = IXTlattice( base, [name], [a], [b], [c],
% !                          [alpha],[beta], [gamma], 'space_group');
%
% !! @author Ibon Bustinduy, ISIS
% !! @V 1.0 Date: 2004/05/13
% 
% !-----------------------------------------------------------------------------------------------------------------------------------
% ! IXTsample
% ! =============
% ! NeXus class: NXsample
% !
% !	base					IXTbase	entry name in NeXus file
% !	a						real	
% !	b						real	
% !	c			real	
% !	alpha						real	
% !	beta						real	
% !	gamma						real
% ! space_group                 char
% !
% !
    lattice.base = IXTbase;
    lattice.a = 0.0;
    lattice.b = 0.0;
    lattice.c = 0.0;
    lattice.alpha = 0.0;
    lattice.beta = 0.0;
    lattice.gamma = 0.0;
    lattice.space_group = 'space group';
    
    lattice = class(lattice,'IXTlattice');
if (nargin > 0)
    lattice = libisisexc('IXTlattice','create',lattice,varargin);
end    


