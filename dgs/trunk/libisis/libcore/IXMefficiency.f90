module IXMefficiency 
 use IXMtype_definitions
 contains
      real(dp) function EFF(En, atms)
      real(dp):: En, atms
!
!  Returns efficiency of helium gas tube for an energy "En" in meV.
!
! Origin of data
! --------------
!  CKL data :
!   "At 2200 m/s xsect=5327 barns    En=25.415 meV         "
!   "At 10 atms, rho_atomic=2.688e-4,  so sigma=1.4323 cm-1"
!
!  These data are not quite consistent, but the errors are small :
!    2200 m/s = 25.299 meV
!    5327 barns & 1.4323 cm-1 ==> 10 atms of ideal gas at 272.9K
!   but at what temperature are the tubes "10 atms" ?
!
!  Shall use  1.4323 cm-1 @ 3.49416 A-1 with sigma prop. 1/v
!
!  This corresponds to a reference energy of 25.299meV, NOT 25.415.
!  This accounts for a difference of typically 1 pt in 1000 for
!  energies around a few hundred meV.
!
!C
!C Modified W.Hahn 7/5/1996
!C	Remove "implicite double precision (a-h, o-z)" statement and 
!C       declare rad,atms,t2rad,sigref,wref,atmref,pi,const,c_eff_f,c_eff_g,wvec,alf,y,eff_f, eff_g, EFFCHB as double precision
!C       and declare i as integer*4 to avoid compilation
!C	warning with /WARNINGS=DECLARATIONS option
!C
      real(dp)::  c_eff_f(25), c_eff_g(25), wvec, alf
      real(dp):: y, eff_f, eff_g
      integer(i4b)::i

      real(dp),parameter:: rad = 0.0127d0, t2rad = 0.063d0,sigref=143.23d0, wref=3.49416d0, atmref=10.0d0
      real(dp),parameter::const=2.0d0*sigref*wref/atmref

      data (c_eff_f(i),i=1,25)/     0.7648360390553052, &
     &     -0.3700950778935237    , 0.1582704090813516, &
     &     -6.0170218669705407E-02, 2.0465515957968953E-02, &
     &     -6.2690181465706840E-03, 1.7408667184745830E-03, &
     &     -4.4101378999425122E-04, 1.0252117967127217E-04, &
     &     -2.1988904738111659E-05, 4.3729347905629990E-06, &
     &     -8.0998753944849788E-07, 1.4031240949230472E-07, &
     &     -2.2815971698619819E-08, 3.4943984983382137E-09, &
     &     -5.0562696807254781E-10, 6.9315483353094009E-11, &
     &     -9.0261598195695569E-12, 1.1192324844699897E-12, & 
     &     -1.3204992654891612E-13, 1.4100387524251801E-14, &
     &     -8.6430862467068437E-16,-1.1129985821867194E-16, &
     &     -4.5505266221823604E-16, 3.8885561437496108E-16/ 
      data (c_eff_g(i),i=1,25)/     2.033429926215546, &
     &     -2.3123407369310212E-02, 7.0671915734894875E-03, &
     &     -7.5970017538257162E-04, 7.4848652541832373E-05, &
     &      4.5642679186460588E-05,-2.3097291253000307E-05, &
     &      1.9697221715275770E-06, 2.4115259271262346E-06, &
     &     -7.1302220919333692E-07,-2.5124427621592282E-07, &
     &      1.3246884875139919E-07, 3.4364196805913849E-08, &
     &     -2.2891359549026546E-08,-6.7281240212491156E-09, &
     &      3.8292458615085678E-09, 1.6451021034313840E-09, &
     &     -5.5868962123284405E-10,-4.2052310689211225E-10, &
     &      4.3217612266666094E-11, 9.9547699528024225E-11, &
     &      1.2882834243832519E-11,-1.9103066351000564E-11, &
     &     -7.6805495297094239E-12, 1.8568853399347773E-12/ 

  
! Program:
! --------
      wvec = sqrt(En/2.0721)
      alf = const*rad*(1.0d0-t2rad)*atms/wvec

      if ( alf .le. 9.0d0 ) then
        eff =(pi_dp/4.0d0)*alf*EFFCHB(0.0d0,10.0d0,c_eff_f,25,alf)
      else if ( alf .ge. 10.0d0 ) then
        y=1.0d0-18.0d0/alf
        eff =1.0d0 - EFFCHB(-1.0d0,1.0d0,c_eff_g,25,y)/alf**2
      else
        eff_f =(pi_dp/4.0d0)*alf*EFFCHB(0.0d0,10.0d0,c_eff_f,25,alf)
        y=1.0d0-18.0d0/alf
        eff_g =1.0d0 - EFFCHB(-1.0d0,1.0d0,c_eff_g,25,y)/alf**2
        eff =(10.0d0-alf)*eff_f  + (alf-9.0d0)*eff_g
      endif

      return
      end function

!---------------------------------------------------------------------
      real(dp) function EFFCHB(a,b,c,m,x)
!
! Essentially CHEBEV of "Numerical Recipes"
!
!C
!C Modified W.Hahn 7/5/1996
!C       Delete "implicite (a-h,o-z)"
!C	Declare m,j as integer*4 and
!C       c, d, dd, y, x, a, b, y2, sv, EFFCH as double precission to avoid compilation
!C	warning with /WARNINGS=DECLARATIONS option
!C

      real(dp):: c(:), d, dd, y, x, a, b, y2, sv
      integer(i4b):: m, j
      d=0.0d0
      dd=0.0d0
      y=(2.0d0*x-a-b)/(b-a)
      y2=2.0d0*y
      do j=m,2,-1
        sv=d
        d=y2*d-dd+c(j)
        dd=sv
      end do
      EFFCHB=y*d-dd+0.5d0*c(1)
      return
      end function
end module IXMefficiency
