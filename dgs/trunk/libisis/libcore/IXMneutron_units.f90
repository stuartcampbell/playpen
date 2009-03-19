!-----------------------------------------------------------------------------------------------------------------------------------
!MODULE: IXMneutron_units
!-----------------------------------------------------------------------------------------------------------------------------------
!! Contains parameters used in the conversion of units
!! @author Toby Perring, ISIS
!! @version $Revision: 1299 $ ($Date: 2008-01-16 09:41:22 -0500 (Wed, 16 Jan 2008) $
!-----------------------------------------------------------------------------------------------------------------------------------
module IXMneutron_units
   use IXMtype_definitions
   use IXMneutron_constants
   implicit none

   integer(i4b), parameter :: n_0=9, n_1=20, n_2=20
   
! Define the unit codes and unit annotations as parameters
! --------------------------------------------------------
   character(len=5),parameter::IXCcode_t='$t   ',IXCcode_v='$v   ',IXCcode_tau='$tau ', &
      &                        IXCcode_lam='$lam ',IXCcode_k='$k   ',IXCcode_e='$e   ',IXCcode_d='$d   '
   character(len=5),parameter::IXCcode_v2='$v2  ',IXCcode_tau2='$tau2',IXCcode_lam2='$lam2', IXCcode_k2='$k2  ',IXCcode_e2='$e2  '
   character(len=5),parameter::IXCcode_v1='$v1  ',IXCcode_tau1='$tau1',IXCcode_lam1='$lam1', IXCcode_k1='$k1  ',IXCcode_e1='$e1  '
   character(len=5),parameter::IXCcode_w='$w   ',IXCcode_wn='$wn  ',IXCcode_thz='$thz '
   character(len=5),parameter::IXCcode_q='$q   ',IXCcode_qplus='$q+  ',IXCcode_qminus='$q-  ', &
      &                        IXCcode_sq='$sq  ',IXCcode_sqplus='$sq+ ',IXCcode_sqminus='$sq- '

   character(len=long_len),parameter:: IXCunit_microsecond='Microsecond', &
      & IXCunit_meter_per_sec='m/s',        IXCunit_sec_per_meter='s/m', &
      & IXCunit_angstrom='Angstrom',       IXCunit_angstrom2='Angstrom^2', &
      & IXCunit_inv_angstrom='Angstrom^-1', IXCunit_inv_angstrom2='Angstrom^-2', &
      & IXCunit_mev='meV',                  IXCunit_thz='THz',                   IXCunit_wn='cm^-1'
     
! Exhaustive list of all unit codes and associated units.
! When a new code is added this array *must* be updated
   integer(i4b),parameter::list_len=26
   character(len=5),parameter:: code_list(list_len)= (/ '$t   ','$v   ','$tau ','$lam ','$k   ','$e   ','$d   ', &
      &   '$v2  ','$tau2','$lam2','$k2  ','$e2  ','$v1  ','$tau1','$lam1','$k1  ','$e1  ', &
      &   '$w   ','$wn  ','$thz ',   &
      &   '$q   ','$q+  ','$q-  ','$sq  ','$sq+ ','$sq- ' /)    

! character array constructor must have same length for all elements
   character(len=long_len),parameter:: units_list(list_len)= (/ &
       'Microsecond', &
       'm/s        ', &
       's/m        ', &
       'Angstrom   ', &
       'Angstrom^-1', &
       'meV        ', &
       'Angstrom   ', &
       'm/s        ', &
       's/m        ', &
       'Angstrom   ', &
       'Angstrom^-1', &
       'meV        ', &
       'm/s        ', &
       's/m        ', &
       'Angstrom   ', &
       'Angstrom^-1', &
       'meV        ', &
       'meV        ', &
       'cm^-1      ', &
       'THz        ', &
       'Angstrom^-1', &
       'Angstrom^-1', &
       'Angstrom^-1', &
       'Angstrom^2 ', &
       'Angstrom^2 ', &
       'Angstrom^2 '  &
   /)

   character(len=long_len),parameter::cap_list(list_len) = (/ &
       'time-of-flight             ', &
       'neutron speed              ', &
       'neutron inverse speed      ', &
       'wavelength                 ', &
       'wavevector                 ', &
       'Energy                     ', &
       'd-spacing                  ', &
       'final neutron speed        ', &
       'final neutron inverse speed', &
       'final wavelength           ', &
       'final wavevector           ', &
       'final energy               ', &
       'final neutron speed        ', &
       'final neutron inverse speed', &
       'final wavelength           ', &
       'final wavevector           ', &
       'incident energy            ', &
       'energy transfer            ', &
       'energy transfer            ', &
       'energy transfer            ', &
       'momentum transfer          ', &
       'momentum transfer          ', &
       'momentum transfer          ', &
       'square of momentum transfer', &
       'square of momentum transfer', &
       'square of momentum transfer'  &
   /)                 
   
! Special units definitions:
   character(len=5),parameter::IXCspecnoC='$spno',IXCworknoC='$wkno',IXCcountsC='$cts '   
   character(len=5),parameter::IXCnullcode='$' ! it's a standard code, just nothing in it
   character(len=long_len),parameter::IXCnullunits=' '


! Define arrays for units conversin routines
!-------------------------------------------
! Elastic instrument units:
   character(len=5),parameter:: u_0(n_0)= (/ '$t   ','$v   ','$tau ','$lam ','$k   ','$e   ','$d   ','$q   ','$sq  ' /)
   real(dp),parameter:: a_0(n_0)= (/ 0, 1,-1,-1, 1, 2,-1, 1, 2 /)
   real(dp),parameter:: b_0(n_0)= (/ 0, 0, 0, 0, 0, 0,-1, 1, 2 /)
   real(dp),parameter:: g_0(n_0)= (/ 1,-1, 1, 1,-1,-2, 1,-1,-2 /)
   real(dp),parameter:: c_0(n_0)= (/ 1.0_dp, 1.0e6_dp, 1.0e-6_dp, c_t_to_lam, c_t_to_k, &
      &                              c_t_to_emev, c_t_to_d, c_t_to_q, c_t_to_sq /)
   integer(i4b),parameter:: qopt_0(n_0)= (/ 0, 0, 0, 0, 0, 0, 0, 0, 0 /)

! Direct geometry units:   
   character(len=5),parameter:: u_1(n_1)= (/ '$t   ','$v   ','$tau ','$lam ','$k   ','$e   ','$v2  ',&
      &        '$tau2','$lam2','$k2  ','$e2  ','$w   ','$wn  ','$thz ',   &
      &        '$q   ','$q+  ','$q-  ','$sq  ','$sq+ ','$sq- ' /)
   real(dp),parameter:: a_1(n_1)= (/ 0, 1,-1,-1, 1, 2, 1,-1,-1, 1, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0 /)
   real(dp),parameter:: b_1(n_1)= (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
   real(dp),parameter:: g_1(n_1)= (/ 1,-1, 1, 1,-1,-2,-1, 1, 1,-1,-2,-2,-2,-2, 0, 0, 0, 0, 0, 0 /)
   real(dp) :: c_1(n_1)
        data c_1/ 1.0_dp, 1.0e6_dp, 1.0e-6_dp, c_t_to_lam, c_t_to_k, &
      &           c_t_to_emev, 1.0e6_dp, 1.0e-6_dp, c_t_to_lam, c_t_to_k, &
      &           c_t_to_emev,  c_t_to_emev, c_t_to_ewav, c_t_to_ethz, 0, 0, &
      &           0, 0, 0, 0 /
   integer(i4b),parameter:: qopt_1(n_1)= (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, -1, 2, 2, -2 /)
! The parameter initialisation statement for c_1 replaced the following which doesn't seem to work with g95
!  real(dp),parameter :: c_1(n_1) = (/ 1.0_dp, 1.0e6_dp, 1.0e-6_dp, &
!      &        c_t_to_lam, c_t_to_k, c_t_to_emev, 1.0e6_dp, 1.0e-6_dp, &
!      &        c_t_to_lam, c_t_to_k, c_t_to_emev,  c_t_to_emev, c_t_to_ewav,&
!      &        c_t_to_ethz, 0, 0, 0, 0, 0, 0 /)

! Indirect geometry units: 
   character(len=5),parameter:: u_2(n_2)= (/ '$t   ','$v   ','$tau ','$lam ','$k   ','$e   ','$v1  ',&
      &        '$tau1','$lam1','$k1  ','$e1  ','$w   ','$wn  ','$thz ',   &
      &        '$q   ','$q+  ','$q-  ','$sq  ','$sq+ ','$sq- ' /)
   real(dp),parameter:: a_2(n_2)= (/ 0, 1,-1,-1, 1, 2, 1,-1,-1, 1, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0 /)
   real(dp),parameter:: b_2(n_2)= (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /)
   real(dp),parameter:: g_2(n_2)= (/ 1,-1, 1, 1,-1,-2,-1, 1, 1,-1,-2,-2,-2,-2, 0, 0, 0, 0, 0, 0 /)
   real(dp):: c_2(n_2)
        data c_2/ 1.0_dp, 1.0e6_dp, 1.0e-6_dp, c_t_to_lam, c_t_to_k, &
      &           c_t_to_emev, 1.0e6_dp, 1.0e-6_dp, c_t_to_lam, c_t_to_k, &
      &           c_t_to_emev,  c_t_to_emev, c_t_to_ewav, c_t_to_ethz, 0, 0, &
      &           0, 0, 0, 0 /
   integer(i4b),parameter:: qopt_2(n_2)= (/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, -1, 2, 2, -2 /)
! The parameter initialisation statement for c_2 replaced the following which doesn't seem to work with g95
!  real(dp),parameter:: c_2(n_2)=(/ 1.0_dp, 1.0e6_dp, 1.0e-6_dp, &
!      &          c_t_to_lam, c_t_to_k, c_t_to_emev, &
!      &          1.0e6_dp, 1.0e-6_dp, c_t_to_lam, c_t_to_k, c_t_to_emev,  &
!      &          c_t_to_emev, c_t_to_ewav, c_t_to_ethz, 0, 0, 0, 0, 0, 0 /)   
   
end module IXMneutron_units
