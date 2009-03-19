! crystal analyzer module
module IXMcrystalanalyser
  use IXMtype_definitions
  use IXMbase

  type IXTcrystalanalyser

     type(IXTbase):: base
     character(len=long_len):: name		!! Name of the crystal
     real(dp):: distance					!! Distance from sample (m) (-ve if upstream of sample)
     real(dp):: energy						!! Energy of reflected neutrons (mev)
     real(dp):: dspace						!! Lattice parameter of nominal reflection (ang)
     integer(i4b):: reflection(3)			!! Reflection [h,k,l]
     real(dp):: rho_h						!! Radius of curvature of crystal in the horizontal plane (m)
     real(dp):: rho_v						!! Radius of curvature of crystal in the vertical plane (m)
     real(dp):: width					!! Width of crystal if rectangular (m)
     real(dp):: height					!! Height of crystal if rectangular (m)

  end type IXTcrystalanalyser

end module IXMcrystalanalyser

