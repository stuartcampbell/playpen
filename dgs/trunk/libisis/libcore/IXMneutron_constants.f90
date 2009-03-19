!-----------------------------------------------------------------------------------------------------------------------------------
!MODULE: IXMneutron_constants
!-----------------------------------------------------------------------------------------------------------------------------------
!! Contains useful constants for neutron scattering. Unless stated otherwise, units are:
!!   - time: microseconds
!!   - distance: meters
!!   - energy: meV
!!   - wavelength: Angstrom
!!   - wavevector: Angstrom^-1 == 2*pi/wavelength
!!
!! For example, c_t_to_d corresponds to 'time-to-d spacing', and can be used as
!!
!!    d = c_t_to_d * t / (L * sin(theta))
!!
!! where t = time-of-flight, L is the total flight-path and theta is half the scattering angle.
!!
!! @author Toby Perring, ISIS
!! @version $Revision: 227 $ ($Date: 2004-07-27 08:44:52 -0400 (Tue, 27 Jul 2004) $)
!! @see IXMphysical_constants
!-----------------------------------------------------------------------------------------------------------------------------------
module IXMneutron_constants
	use IXMtype_definitions
	use IXMphysical_constants
	implicit none

	real(dp), parameter :: c_t_to_k  = 1.0e3_dp*neutron_mass_mantissa/hbar_mantissa
	real(dp), parameter :: c_t_to_lam= twopi_dp/c_t_to_k
	real(dp), parameter :: c_t_to_emev = 0.5e7_dp*neutron_mass_mantissa/electron_charge_mantissa
	real(dp), parameter :: c_t_to_ewav = 0.5e9_dp*neutron_mass_mantissa/(twopi_dp*hbar_mantissa*speed_of_light_mantissa)
	real(dp), parameter :: c_t_to_ethz = 0.5e7_dp*neutron_mass_mantissa/(twopi_dp*hbar_mantissa)
	real(dp), parameter :: c_t_to_q  = 2.0_dp*c_t_to_k
	real(dp), parameter :: c_t_to_sq = c_t_to_q**2
	real(dp), parameter :: c_t_to_d  = twopi_dp/c_t_to_q

	real(dp), parameter :: c_emev_to_ewav = 100.0_dp*electron_charge_mantissa/(twopi_dp*hbar_mantissa*speed_of_light_mantissa)
	real(dp), parameter :: c_emev_to_ethz = electron_charge_mantissa/(twopi_dp*hbar_mantissa)
	real(dp), parameter :: c_v_to_emev = 5.0e-6_dp*neutron_mass_mantissa/electron_charge_mantissa
	real(dp), parameter :: c_k_to_emev = 5.0_dp*hbar_mantissa*hbar_mantissa/(neutron_mass_mantissa*electron_charge_mantissa)

end module IXMneutron_constants
