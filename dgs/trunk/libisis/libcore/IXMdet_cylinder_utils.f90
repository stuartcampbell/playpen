!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMdet_cylinder_utils
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Toby Perring, ISIS
!! @version $Revision: 778 $ ($Date: 2006-08-18 04:48:03 -0400 (Fri, 18 Aug 2006) $)
!!
!! Module containing utility routines for cylindrical detectors. No assumption is made about the nature
!! of the absorbing material in the cylinder; that will be done in the routines that are specific to
!! particular types of detector - what is needed here is the mean free path for absorption and the 
!! knowledge of the particular geometry of the detector.

module IXMdet_cylinder_utils
  use IXMtype_definitions
  use IXMstatus
  implicit none
  private
  public :: effic_cyl
  
  interface chebev
    module procedure chebev_scalar, chebev_vector
  end interface
  
contains

!-----------------------------------------------------------------------------------------------------------------------

    subroutine effic_cyl (rad, sigma_macro, theta, effic, dx, vx, vy)
    implicit none
    real(dp), intent(IN) :: rad                 !! Radius of cylindrical tube (m)
    real(dp), intent(IN) :: sigma_macro         !! Macroscopic absorption cross-section (m^-1)
    real(dp), intent(IN) :: theta               !! Angle of incident beam from perpendicular to cylinder axis (rad)
    real(dp), intent(OUT), optional :: effic    !! Efficiency of detector (0-1)
    real(dp), intent(OUT), optional :: dx       !! Mean depth of absorption w.r.t. tube centre along path defined by theta (m)
    real(dp), intent(OUT), optional :: vx       !! Variance of depth of absorption along path defined by theta (m)
    real(dp), intent(OUT), optional :: vy       !! Variance of width of absorption (m)
    
    real(dp) rad_long, alf, effic_temp, dx_temp, vx_temp, vy_temp
    
    rad_long = rad/cos(theta)   ! ellipse halfwidth along beam flightpath
    alf = 2.0_dp*sigma_macro*rad_long

! We want to minimise unnecessary calculation of what is a fairly expensive calculation - but not have the ridiculous
! amount of coding that optimises every permutation of the five possible output arguments
! Assume that one want the zeroth, first or second moments 
    if (.not.(present(vx).or.present(vy))) then
        if (present(effic).and.(.not.present(dx))) then
            call effic_cyl_internal (alf, effic=effic)
        elseif ((.not.present(effic)).and.present(dx)) then
            call effic_cyl_internal (alf, dx=dx_temp)
            dx = rad_long*dx_temp
        elseif (present(effic).and.present(dx)) then
            call effic_cyl_internal (alf, effic=effic, dx=dx_temp)
            dx = rad_long*dx_temp
        endif
    else
        call effic_cyl_internal (alf, effic=effic_temp, dx=dx_temp, vx=vx_temp, vy=vy_temp)
        if (present(effic)) effic = effic_temp
        if (present(dx))     dx = rad_long*dx_temp
        if (present(vx))     vx =(rad_long**2)*vx_temp
        if (present(vy))     vy = rad*vy_temp
    endif
    
    end subroutine effic_cyl

!-----------------------------------------------------------------------------------------------------------------------
! Calculate various moments of absorption in a cylindical tube. Calculations done from first principles (see notes
! by Toby Perring), and expressed as Taylor series, assymptotic expansions (large absoprtion), or linear combination
! of both in overlap region. From testing against numerical integrations (performed with NAG algorithm with
! convergence checks and accuracy criteria) I believe that they have relative accuracy of about 1 pt in 10^12.
!
! In the case of variance of depth of absorption, the assymptotic expansion was not performed - but instead calculated
! fromt those for coefficients of efficiency, mean depth and mean depth squared (see notes).

    subroutine effic_cyl_internal (alf, effic, dx, xsqr, vx, vy)
    implicit none
    real(dp), intent(IN) :: alf                 !! Diameter of cylindrical tube in inverse macroscopic absorption cross-section
    real(dp), intent(OUT), optional :: effic    !! Efficiency of detector (0-1)
    real(dp), intent(OUT), optional :: dx       !! Mean depth of absorption w.r.t. tube centre (units: radius)
    real(dp), intent(OUT), optional :: xsqr     !! Mean square depth of absorption w.r.t. tube centre (units: radius^2)
    real(dp), intent(OUT), optional :: vx       !! Variance of depth of absorption (units: radius^2)
    real(dp), intent(OUT), optional :: vy       !! Variance of width of absorption (units: radius^2)
    
    real(dp) alf_in, y, eff_F, eff_g, dx_f, dx_g, xsqr_f, xsqr_g, vx_f, vx_g, vy_f, vy_g
    
    real(dp), parameter:: g0=(32.0_dp-3.0_dp*(pi_dp**2))/48.0_dp, g1=14.0_dp/3.0_dp-(pi_dp**2)/8.0_dp

    real(dp), parameter :: c_eff_f(25) = (/0.7648360390553052_dp,     -0.3700950778935237_dp    , 0.1582704090813516_dp,     &
     &         -6.0170218669705407e-02_dp, 2.0465515957968953e-02_dp, -6.2690181465706840e-03_dp, 1.7408667184745830e-03_dp, &
     &         -4.4101378999425122e-04_dp, 1.0252117967127217e-04_dp, -2.1988904738111659e-05_dp, 4.3729347905629990e-06_dp, &
     &         -8.0998753944849788e-07_dp, 1.4031240949230472e-07_dp, -2.2815971698619819e-08_dp, 3.4943984983382137e-09_dp, &
     &         -5.0562696807254781e-10_dp, 6.9315483353094009e-11_dp, -9.0261598195695569e-12_dp, 1.1192324844699897e-12_dp, &
     &         -1.3204992654891612e-13_dp, 1.4100387524251801e-14_dp, -8.6430862467068437e-16_dp,-1.1129985821867194e-16_dp, &
     &         -4.5505266221823604e-16_dp, 3.8885561437496108e-16_dp/)
     
    real(dp), parameter :: c_eff_g(25) = (/2.033429926215546_dp,      -2.3123407369310212e-02_dp, 7.0671915734894875e-03_dp, &
     &         -7.5970017538257162e-04_dp, 7.4848652541832373e-05_dp,  4.5642679186460588e-05_dp,-2.3097291253000307e-05_dp, &
     &          1.9697221715275770e-06_dp, 2.4115259271262346e-06_dp, -7.1302220919333692e-07_dp,-2.5124427621592282e-07_dp, &
     &          1.3246884875139919e-07_dp, 3.4364196805913849e-08_dp, -2.2891359549026546e-08_dp,-6.7281240212491156e-09_dp, &
     &          3.8292458615085678e-09_dp, 1.6451021034313840e-09_dp, -5.5868962123284405e-10_dp,-4.2052310689211225e-10_dp, &
     &          4.3217612266666094e-11_dp, 9.9547699528024225e-11_dp, 1.2882834243832519e-11_dp,-1.9103066351000564e-11_dp, &
     &         -7.6805495297094239e-12_dp, 1.8568853399347773e-12_dp/)

    real(dp), parameter :: c_dx_f(25) = (/1.457564928500728_dp,      -0.2741263150129247_dp,     1.4102406058428482E-02_dp, &
     &         1.1868136977190956e-02_dp,-4.7000120888695418e-03_dp,  6.7071002620380348e-04_dp, 1.2315212155928235e-04_dp, &
     &        -8.7985748380390304e-05_dp, 1.8952644758594150e-05_dp,  4.4101711646149510e-07_dp,-1.5292393205490473e-06_dp, &
     &         4.5050196748941396e-07_dp,-2.9971703975339992e-08_dp, -2.3573145628841274e-08_dp, 9.6228336343706644e-09_dp, &
     &        -1.3038786850216866e-09_dp,-2.9423462000188749e-10_dp,  1.8813720970012326e-10_dp,-3.7682054143672871e-11_dp, &
     &        -1.9125961925325896e-12_dp, 3.3516145414580478e-12_dp, -9.0842416922143343e-13_dp, 4.3951786654616853e-14_dp, &
     &         4.5793924208226145e-14_dp,-1.4916540225229369e-14_dp/)

    real(dp), parameter :: c_dx_g(25) = (/1.980495234559052_dp,       1.3148750635418816e-02_dp,-3.5137830163154959e-03_dp, &
     &         1.4111112411286597e-04_dp,-2.4707009281715875e-05_dp, -4.9602024972950076e-08_dp, 1.5268651833078018e-06_dp, &
     &        -4.8070752083129165e-07_dp,-3.5826648758785495e-08_dp,  6.0264253483044428e-08_dp,-4.2948016776289677e-09_dp, &
     &        -7.5840171520624722e-09_dp, 1.0468151234732659e-09_dp,  1.1267346944343615e-09_dp,-1.4810551229871294e-10_dp, &
     &        -1.9605287726598419e-10_dp, 9.8596597553068932e-12_dp,  3.6752354493074790e-11_dp, 3.2634850377633029e-12_dp, &
     &        -6.6207839211074316e-12_dp,-1.9158341579839089e-12_dp,  9.6091495871419851e-13_dp, 6.3198529742791721e-13_dp, &
     &        -6.4681177081027385e-14_dp,-1.8198241524824965e-13_dp/)

    real(dp), parameter :: c_xsqr_f(25) = (/2.675986138240137_dp,     0.4041429091631520_dp,     2.1888771714164858e-02_dp, &
     &        -3.4310286472213617e-02_dp, 9.8724790919419380e-03_dp, -7.7267251256297631e-04_dp,-4.6681418487147020e-04_dp, &
     &         2.0604262514245964e-04_dp,-3.1387761886573218e-05_dp, -5.1728966665387510e-06_dp, 3.9417564710109155e-06_dp, &
     &        -8.6522505504893487e-07_dp,-1.6220695979729527e-08_dp,  6.8546255754808882e-08_dp,-2.0405647520593817e-08_dp, &
     &         1.4047699248287415e-09_dp, 1.0523175986154598e-09_dp, -4.3422357653977173e-10_dp, 5.9649738481937220e-11_dp, &
     &         1.3017424915773290e-11_dp,-8.4605289440986553e-12_dp,  1.7046483669069801e-12_dp, 8.2185647176657995e-14_dp, &
     &        -1.4448442442471787e-13_dp, 3.5720454372167865e-14_dp/)

    real(dp), parameter :: c_xsqr_g(25) = (/1.723549588238691_dp,     0.1365565801015080_dp,     2.0457962179522337e-03_dp, &
     &        -3.9875695195008110e-04_dp, 2.3949621855833269e-05_dp, -1.6129278268772751e-06_dp,-1.1466609509480641e-06_dp, &
     &         4.3086322193297555e-07_dp, 1.7612995328875059e-09_dp, -4.5839686845239313e-08_dp, 5.9957170539526316e-09_dp, &
     &         5.3204258865235943e-09_dp,-1.1050097059595032e-09_dp, -7.7028480982566094e-10_dp, 1.5644044393248180e-10_dp, &
     &         1.3525529252156332e-10_dp,-1.5409274967126407e-11_dp, -2.6052305868162762e-11_dp,-8.3781981352615275e-13_dp, &
     &         4.8823761700234058e-12_dp, 1.1086589979392158e-12_dp, -7.5851658287717783e-13_dp,-4.0599884565395428e-13_dp, &
     &         7.9971584909799275e-14_dp, 1.3500020545897939e-13_dp/)

    real(dp), parameter :: c_vx_f(25) = (/1.226904583058190_dp,      -0.3621914072547197_dp,     6.0117947617747081e-02_dp, &
     &         1.8037337764424607e-02_dp,-1.4439005957980123e-02_dp,  3.8147446724517908e-03_dp, 1.3679160269450818e-05_dp, &
     &        -3.7851338401354573e-04_dp, 1.3568342238781006e-04_dp, -1.3336183765173537e-05_dp,-7.5468390663036011e-06_dp, &
     &         3.7919580869305580e-06_dp,-6.4560788919254541e-07_dp, -1.0509789897250599e-07_dp, 9.0282233408123247e-08_dp, &
     &        -2.1598200223849062e-08_dp,-2.6200750125049410e-10_dp,  1.8693270043002030e-09_dp,-6.0097600840247623e-10_dp, &
     &         4.7263196689684150e-11_dp, 3.3052446335446462e-11_dp, -1.4738090470256537e-11_dp, 2.1945176231774610e-12_dp, &
     &         4.7409048908875206e-13_dp,-3.3502478569147342e-13_dp/)
     
    real(dp), parameter :: c_vx_g(25) = (/1.862646413811875_dp,       7.5988886169808666e-02_dp,-8.3110620384910993e-03_dp, &
     &         1.1236935254690805e-03_dp,-1.0549380723194779e-04_dp, -3.8256672783453238e-05_dp, 2.2883355513325654e-05_dp, &
     &        -2.4595515448511130e-06_dp,-2.2063956882489855e-06_dp,  7.2331970290773207e-07_dp, 2.2080170614557915e-07_dp, &
     &        -1.2957057474505262e-07_dp,-2.9737380539129887e-08_dp,  2.2171316129693253e-08_dp, 5.9127004825576534e-09_dp, &
     &        -3.7179338302495424e-09_dp,-1.4794271269158443e-09_dp,  5.5412448241032308e-10_dp, 3.8726354734119894e-10_dp, &
     &        -4.6562413924533530e-11_dp,-9.2734525614091013e-11_dp, -1.1246343578630302e-11_dp, 1.6909724176450425e-11_dp, &
     &         5.6146245985821963e-12_dp,-2.7408274955176282e-12_dp/)

    real(dp), parameter :: c_vy_f(25) = (/2.408884004758557_dp,       0.1441097208627301_dp,    -5.0093583831079742e-02_dp, &
     &         1.0574012517851051e-02_dp,-4.7245491418700381e-04_dp, -5.6874753986616233e-04_dp, 2.2050994176359695e-04_dp, &
     &        -3.0071128379836054e-05_dp,-6.5175276460682774e-06_dp,  4.2908624511150961e-06_dp,-8.8327783029362728e-07_dp, &
     &        -3.5778896608773536e-08_dp, 7.6164115048182878e-08_dp, -2.1399959173606931e-08_dp, 1.1599700144859781e-09_dp, &
     &         1.2029935880786269e-09_dp,-4.6385151497574384e-10_dp,  5.7945164222417134e-11_dp, 1.5725836188806852e-11_dp, &
     &        -9.1953450409576476e-12_dp, 1.7449824918358559e-12_dp,  1.2301937246661510e-13_dp,-1.6739387653785798e-13_dp, &
     &         4.5505543777579760e-14_dp,-4.3223757906218907e-15_dp/)
     
    real(dp), parameter :: c_vy_g(25) = (/1.970558139796674_dp,       1.9874189524780751e-02_dp,-5.3520719319403742e-03_dp, &
     &         2.3885486654173116e-04_dp,-4.1428357951582839e-05_dp, -6.3229035418110869e-07_dp, 2.8594609307941443e-06_dp, &
     &        -8.5378305322625359e-07_dp,-8.2383358224191738e-08_dp,  1.1218202137786015e-07_dp,-6.0736651874560010e-09_dp, &
     &        -1.4453200922748266e-08_dp, 1.7154640064021009e-09_dp,  2.1673530992138979e-09_dp,-2.4074988114186624e-10_dp, &
     &        -3.7678839381882767e-10_dp, 1.1723938486696284e-11_dp,  7.0125182882740944e-11_dp, 7.5127332133106960e-12_dp, &
     &        -1.2478237332302910e-11_dp,-3.8880659802842388e-12_dp,  1.7635456983633446e-12_dp, 1.2439449470491581e-12_dp, &
     &        -9.4195068411906391e-14_dp,-3.4105815394092076e-13_dp/)

    alf_in = max(alf,0.0_dp)   ! -ve alf is treated as alf=0 i.e. no detector
    if (alf_in <= 9.0_dp) then
        if (present(effic)) effic=(pi_dp/4.0_dp)*alf_in*chebev(0.0_dp,10.0_dp,c_eff_f,alf_in)
        if (present(dx ))   dx   =-0.125_dp*alf_in*chebev(0.0_dp,10.0_dp,c_dx_f,alf_in)
        if (present(xsqr))  xsqr = 0.25_dp*chebev(0.0_dp,10.0_dp,c_xsqr_f,alf_in)
        if (present(vx))    vx   = 0.25_dp*chebev(0.0_dp,10.0_dp,c_vx_f,alf_in)
        if (present(vy))    vy   = 0.25_dp*chebev(0.0_dp,10.0_dp,c_vy_f,alf_in)
    else if ( alf_in >= 10.0_dp ) then
        y=1.0_dp-18.0_dp/alf_in
        eff_g =1.0_dp - chebev(-1.0_dp,1.0_dp,c_eff_g,y)/alf_in**2
        if (present(effic)) effic = eff_g
        if (present(dx ))   dx =(2.0_dp*chebev(-1.0_dp,1.0_dp,c_dx_g,y)/alf_in - 0.25_dp*pi_dp) / eff_g
        if (present(xsqr))  xsqr=((-pi_dp/alf_in)*chebev(-1.0_dp,1.0_dp,c_xsqr_g,y) + 2.0_dp/3.0_dp ) / eff_g
        if (present(vx))    vx  =g0 + g1*chebev(-1.0_dp,1.0_dp,c_vx_g,y)/(alf_in**2)
        if (present(vy))    vy  =( -chebev(-1.0_dp,1.0_dp,c_vy_g,y)/(alf_in**2) + 1.0_dp/3.0_dp ) / eff_g
    else
        y=1.0_dp-18.0_dp/alf_in
        eff_g =1.0_dp - chebev(-1.0_dp,1.0_dp,c_eff_g,y)/alf_in**2
        if (present(effic)) then
            eff_f =(pi_dp/4.0_dp)*alf_in*chebev(0.0_dp,10.0_dp,c_eff_f,alf_in)
            effic =(10.0_dp-alf_in)*eff_f  + (alf_in-9.0_dp)*eff_g
        endif
        if (present(dx)) then
            dx_f =-0.125_dp*alf_in*chebev(0.0_dp,10.0_dp,c_dx_f,alf_in)
            dx_g =(2.0_dp*chebev(-1.0_dp,1.0_dp,c_dx_g,y)/alf_in - 0.25_dp*pi_dp) / eff_g
            dx =(10.0_dp-alf_in)*dx_f  + (alf_in-9.0_dp)*dx_g
        endif
        if (present(xsqr)) then
            xsqr_f=0.25_dp*chebev(0.0_dp,10.0_dp,c_xsqr_f,alf_in)
            xsqr_g=((-pi_dp/alf_in)*chebev(-1.0_dp,1.0_dp,c_xsqr_g,y) + 2.0_dp/3.0_dp ) / eff_g
            xsqr=(10.0_dp-alf_in)*xsqr_f + (alf_in-9.0_dp)*xsqr_g
        endif
        if (present(vx)) then
            vx_f  =0.25_dp*chebev(0.0_dp,10.0_dp,c_vx_f,alf_in)
            vx_g  =g0 + g1*chebev(-1.0_dp,1.0_dp,c_vx_g,y)/(alf_in**2)
            vx  =(10.0_dp-alf_in)*vx_f   + (alf_in-9.0_dp)*vx_g
        endif
        if (present(vy)) then
            vy_f  =0.25_dp*chebev(0.0_dp,10.0_dp,c_vy_f,alf_in)
            vy_g  =( -chebev(-1.0_dp,1.0_dp,c_vy_g,y)/(alf_in**2) + 1.0_dp/3.0_dp ) / eff_g
            vy  =(10.0_dp-alf_in)*vy_f   + (alf_in-9.0_dp)*vy_g
        endif
    endif

    return
    end subroutine effic_cyl_internal
    

!----------------------------------------------------------------------------------------------------------------------
! Coefficients of eries constructed using Numerical recipes routines. Need routines to evaluate series expansions,
! so use the following - essentially CHEBEV of "Numerical Recipes"

    function chebev_scalar(a,b,coeff,x)
    use IXMtype_definitions
    real(dp), intent(IN):: a,b,coeff(:),x
    real(dp) :: chebev_scalar
    integer(i4b) j,m
    real(dp) d,dd,sv,y,y2

    d=0.0_dp
    dd=0.0_dp
    y=(2.0_dp*x-a-b)/(b-a)
    y2=2.0_dp*y
    do j=m,2,-1
      sv=d
      d=y2*d-dd+coeff(j)
      dd=sv
    end do
    chebev_scalar=y*d-dd+0.5_dp*coeff(1)
    return
    end function chebev_scalar

!----------------------------------------------------------
    function chebev_vector(a,b,coeff,x)
    use IXMtype_definitions
    real(dp), intent(IN):: a,b,coeff(:),x(:)
    real(dp) :: chebev_vector(size(x))
    integer(i4b) j,m
    real(dp), dimension(size(x)) :: d,dd,sv,y,y2

    d=0.0_dp
    dd=0.0_dp
    y=(2.0_dp*x-a-b)/(b-a)
    y2=2.0_dp*y
    do j=m,2,-1
      sv=d
      d=y2*d-dd+coeff(j)
      dd=sv
    end do
    chebev_vector=y*d-dd+0.5_dp*coeff(1)
    return
    end function chebev_vector

    
end module IXMdet_cylinder_utils



