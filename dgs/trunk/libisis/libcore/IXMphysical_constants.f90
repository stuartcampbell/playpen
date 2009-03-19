!! Fundamental physical constants : CODATA reecommended values for 1998 
!! (http://physics.nist.gov/cuu/Constants/)
!! @author Toby Perring, ISIS
!! @version $Revision: 596 $ ($Date: 2006-01-18 13:30:00 -0500 (Wed, 18 Jan 2006) $)
!! @see IXMneutron_constants
module IXMphysical_constants
	use IXMtype_definitions
	implicit none

	real(dp), parameter :: neutron_mass=1.67492716e-27_dp
	real(dp), parameter :: neutron_mass_mantissa=1.67492716_dp
	real(dp), parameter :: hbar=1.054571596e-34_dp
	real(dp), parameter :: hbar_mantissa=1.054571596_dp
	real(dp), parameter :: electron_charge=1.602176462e-19_dp
	real(dp), parameter :: electron_charge_mantissa=1.602176462_dp
	real(dp), parameter :: speed_of_light=2.99792458e8_dp	!! speed of light (m s-2)
	real(dp), parameter :: speed_of_light_mantissa=2.99792458_dp

end module IXMphysical_constants
