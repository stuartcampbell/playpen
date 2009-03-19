! sample module
module IXMsample
  use IXMbase
  use IXMlattice
  implicit none
  public ::	IXTsample
  type IXTsample
     private
     type(IXTbase):: base 
     character(len=long_len)::	name='name'				!! Name	of sample e.g. 'bilayer	manganite, Kimura no. 3'
     character(len=long_len)::chemical_formula='formula'	!! Sample chemical formula e.g.	'La1.4Sr1.6Mn2O7'
     real(dp)::	temperature=0.0_dp						!! Temperature in Kelvin
     real(dp)::	electric_field(3)=0.0_dp				!! Electric	field in V/m, in one of	the	conventional coordinate	frames below.
     character(len=short_len)::electric_coord='coord'	!! Coordinate frame	in which electric field	is expressed ('instrument'|'rlu')
     real(dp)::	magnetic_field(3)=0.0_dp				!! Applied magnetic	field in Tesla in coordinate frame below.
     character(len=short_len)::magnetic_coord='coord'	!! Coordinate frame	in which magnetic field	is expressed ('instrument'|'rlu')
     real(dp)::	pressure=0.0_dp						!! Applied hydrostatic pressure

     type(IXTlattice)::lattice !!contains lattice parameters ->	a,b,c, alpha,beta,gamma	and	space_group

     !Bravais lattice symmetry	e.g. 'orthorhombic', 'cubic', 'hexagonal' (14 -	one	for	each Bravais lattice)
     !character(len=short_len):: bravais

     real(dp)::	uvec(3)=0.0_dp				!! First reciprocal	lattice	vector defining	scattering plane
     real(dp)::	vvec(3)=0.0_dp				!! Second reciprocal lattice vector	defining scattering	plane
     real(dp)::	psi=0.0_dp					!! Angle of	uvec with respect to incident beam on sample (radians)
     !! Angle of	minor goniometer arc with respect to nominal (i.e. uncorrected)
     !! direction of	uvec
     real(dp)::	omega=0.0_dp
     !! Corrections to crystal orientation (gl, gs, dpsi), where
     !!	   - gl	  major	goniometer arc (radian)
     !!	   - gs	  minor	goniometer arc (radian)
     !!	   - dpsi correction to	psi	(radian)
     real(dp)::	gonio(3)=0.0_dp		
     !! Shape of	sample ('plate', 'sphere', 'cylinder', 'hollow cylinder')
     character(len=short_len)::	shape='shape'
     real(dp)::	x_geom(3)=0.0_dp			!! Sample geometry x-axis (r.l.u.)
     real(dp)::	y_geom(3)=0.0_dp			!! Sample geometry y-axis (r.l.u.)
     !! Position	of sample with respect to nominal sample position. The coordinate frame	is the
     !! spectrometer	frame i.e. z axis along	the	incident beam direction, the y axis	vertically upwards
     real(dp)::	position(3)=0.0_dp
     real(dp)::	dimensions(3)=0.0_dp		!! Plate sample	size: full widths along	sample geometry	x, y, z	axes (m).
     real(dp)::	radius=0.0_dp				!! (Outer) adius of	sample if sphere or	(hollow) cylinder (m).
     real(dp)::	inner_radius=0.0_dp		!! Inner radius	of sample if hollow	cylinder (m).
     real(dp)::	height=0.0_dp				!! Height of sample	if cylinder	(m).
     real(dp)::	mass=0.0_dp				!! Mass	of sample (kg).
     real(dp)::	molecular_weight=0.0_dp	!! Molecular weight	per	formula	unit.
     real(dp)::	xcoh=0.0_dp				!! Coherent	cross-section (barn	per	formula	unit).
     real(dp)::	xinc=0.0_dp				!! Incoherent cross-section	(barn per formula unit).
     real(dp)::	xabs=0.0_dp				!! Absorption cross-section	at 25 meV (barn	per	formula	unit).
  end type IXTsample

#define IXD_TYPE sample
#include "class_header.f90"

contains

#define IXD_DESCRIPTION	"IXTsample class"
#define IXD_TYPE sample
#define IXD_SQTYPE 'sample'
#include "class_base.f90"

  !-----------------------------------------------------------------------------------------------------------------------
  !! IXFcheck will make internal consistency checks in the object, such as array length checking to make
  !! sure the object is properly filled.

  subroutine IXFcheck_sample(sample,status)
    implicit none
    type(IXTsample)::sample
    type(IXTstatus)::status

    call IXFcheck_base(sample%base,status)
    call IXFcheck(sample%lattice,status)

  end subroutine IXFcheck_sample

  !-----------------------------------------------------------------------------------------------------------------------
  ! All classes must provide this operation; it loops through
  ! all members of the class doing the supplied operation

  recursive subroutine IXFoperation_run_sample(op, field, arg, status)
    implicit none
    type(IXTsample) :: arg
    type(IXToperation) :: op
    type(IXTstatus) :: status
    character(len=*) :: field
    logical::cont_op
    call IXFoperationStart(op, 'IXTsample', field, status,arg%base,cont_op)
    if(.not. cont_op)return
    ! this order must match the declaration order in matlab as it is
    ! used when parsing arguments passed in class creation with vargin
    call IXFoperation_run(op, 'base', arg%base, status)
    call IXFoperation_run(op,'name', arg%name, status)
    call IXFoperation_run(op,'chemical_formula',arg%chemical_formula,status)
    call IXFoperation_run(op,'temperature', arg%temperature, status)
    call IXFoperation_run(op,'electric_field', arg%electric_field, status)
    call IXFoperation_run(op,'electric_coord', arg%electric_coord, status)
    call IXFoperation_run(op,'magnetic_field',arg%magnetic_field,status)
    call IXFoperation_run(op,'magnetic_coord', arg%magnetic_coord, status)
    call IXFoperation_run(op,'pressure',arg%pressure,status)
    call IXFoperation_run(op,'lattice',arg%lattice,status)
    call IXFoperation_run(op,'uvec',arg%uvec,status)
    call IXFoperation_run(op,'vvec',arg%vvec,status)
    call IXFoperation_run(op,'psi', arg%psi, status)
    call IXFoperation_run(op,'omega', arg%omega, status)
    call IXFoperation_run(op,'gonio',arg%gonio,status)
    call IXFoperation_run(op,'shape', arg%shape, status)
    call IXFoperation_run(op,'x_geom', arg%x_geom, status)
    call IXFoperation_run(op,'y_geom',arg%y_geom,status)
    call IXFoperation_run(op,'position', arg%position, status)
    call IXFoperation_run(op,'dimensions', arg%dimensions, status)
    call IXFoperation_run(op,'radius',arg%radius,status)
    call IXFoperation_run(op,'inner_radius', arg%inner_radius, status)
    call IXFoperation_run(op,'height', arg%height, status)
    call IXFoperation_run(op,'mass',arg%mass,status)
    call IXFoperation_run(op,'molecular_weight', arg%molecular_weight, status)
    call IXFoperation_run(op,'xcoh', arg%xcoh, status)
    call IXFoperation_run(op,'xinc',arg%xinc,status)
    call IXFoperation_run(op,'xabs', arg%xabs, status)
    call IXFoperationFinish(op, field, status)
  end subroutine IXFoperation_run_sample

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFcreate subroutine STRICTLY takes all the elements required to define a class and creates the resulting
  !! object. The IXTbase type is an optional argument, if it is not supplied the base type from the default IXTtestclass
  !! declaration will be used, it is therefore the last argument. If an element of the object is another class then
  !! it MUST be initialised. 

  subroutine IXFcreate_sample(sample,name,chemical_formula,temperature,electric_field,electric_coord,  &
       magnetic_field,magnetic_coord,pressure,lattice,uvec,vvec,psi,omega,gonio,shape,x_geom,y_geom,position,dimensions, &
       radius,inner_radius,height,mass,molecular_weight,xcoh,xinc,xabs,status)
    implicit none
    type(IXTsample)::sample
    type(IXTstatus)::status  
    type(IXTlattice),intent(in)::lattice
    character(len=*),intent(in)::name,chemical_formula
    character(len=*),intent(in)::electric_coord,magnetic_coord,shape
    real(dp),intent(in)::temperature,electric_field(3),magnetic_field(3),pressure,uvec(3),vvec(3),psi,omega,   &
         gonio(3),x_geom(3),y_geom(3),position(3),dimensions(3),radius,inner_radius,height,mass,molecular_weight, &
         xcoh,xinc,xabs  

    ! nested objects should be tested for initialisation, this shows they have been created properly   
    if( IXFvalid(lattice) .neqv. .true.)then
       call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
            IXCerr_outofmem, 'IXTlattice failure, all nested objects MUST be initialised (IXFcreate_sample)')
    endif
    if(status == IXCseverity_error)return

    ! the set routine can ONLY be called on an initialised object
    ! so in this *special* case it is initialised before it is filled
    call IXFmark_valid(sample)

    call IXFset_sample(sample,status,name,chemical_formula,temperature,electric_field,electric_coord,  &
            magnetic_field,magnetic_coord,pressure,lattice,uvec,vvec,psi,omega,gonio,shape,x_geom,y_geom,position,dimensions, &
            radius,inner_radius,height,mass,molecular_weight,xcoh,xinc,xabs)

  end subroutine IXFcreate_sample

  !-----------------------------------------------------------------------------------------------------------------------
  !!The IXFset operation can only be performed on a properly filled or initialised object. It takes an optional number of
  !! arguments to modify the object contents. A check is made at the end to determine that the edited object is correctly
  !! formed. Error flags are raised if there is any inconsistency. The optional arguments must always be specified by keywords.
  !! The order of arguments should match the order of declaration, except for the IXTbase type which will be the penultimate argument.
  !! The 'ref' argument is used to copy the values of a reference object to another. In this case the object being modified 
  !!does not have to be initialised, but the reference object must be initialised instead.

  recursive subroutine IXFset_sample(sample,status,name,chemical_formula,temperature,electric_field,electric_coord,  &
       magnetic_field,magnetic_coord,pressure,lattice,uvec,vvec,psi,omega,gonio,shape,x_geom,y_geom,position,dimensions, &
       radius,inner_radius,height,mass,molecular_weight,xcoh,xinc,xabs,ref)
    implicit none

    type(IXTsample),intent(inout)::sample
    type(IXTsample),optional,intent(in)::ref
    type(IXTlattice),optional,intent(in)::lattice
    character(len=*),optional,intent(in)::name,chemical_formula
    character(len=*),optional,intent(in)::electric_coord,magnetic_coord,shape
    real(dp),optional,intent(in)::temperature,electric_field(3),magnetic_field(3),pressure,uvec(3),vvec(3),psi,omega,   &
         gonio(3),x_geom(3),y_geom(3),position(3),dimensions(3),radius,inner_radius,height,mass,molecular_weight, &
         xcoh,xinc,xabs
    type(IXTstatus)::status

    ! check that either the reference object is initialised
    ! or that object to be modified is initialised
    if(present(ref))then
       if (IXFvalid(ref) .neqv. .true.)then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Reference object MUST be initialised (IXFset_sample)')
       endif
       if(status == IXCseverity_error)return
       ! now initialise object to be modified, not necessary to check its value
       call IXFmark_valid(sample)
    else    
       if(IXFvalid(sample) .neqv. .true.) then
          call IXFadd_status(status, IXCfacility_libisis, IXCseverity_error, &
               IXCerr_outofmem, 'Set can only be called on an initialised object (IXFset_sample)')
       endif
       if(status == IXCseverity_error)return
    endif

    if (present(ref))then
       call IXFset_sample(sample,status,ref%name,&
            ref%chemical_formula,ref%temperature,&
            ref%electric_field,ref%electric_coord,  &
            ref%magnetic_field,ref%magnetic_coord,ref%pressure,ref%lattice,&
            ref%uvec,ref%vvec,ref%psi,ref%omega,ref%gonio,ref%shape, &
            ref%x_geom, ref%y_geom,ref%position,ref%dimensions,ref%radius,&
            ref%inner_radius,ref%height,ref%mass,ref%molecular_weight,& 
            ref%xcoh,ref%xinc,ref%xabs)
    endif

    if (present(name))sample%name=name
    if (present(chemical_formula))sample%chemical_formula=chemical_formula
    if (present(temperature))sample%temperature=temperature  
    if (present(electric_field))sample%electric_field=electric_field
    if (present(electric_coord))sample%electric_coord=electric_coord
    if (present(magnetic_field))sample%magnetic_field=magnetic_field
    if (present(magnetic_coord))sample%magnetic_coord=magnetic_coord
    if (present(pressure))sample%pressure=pressure
    if (present(lattice))call IXFcopy(lattice,sample%lattice,status)
    if (present(uvec))sample%uvec=uvec
    if (present(vvec))sample%vvec=vvec
    if (present(psi))sample%psi=psi
    if (present(omega))sample%omega=omega
    if (present(gonio))sample%gonio=gonio
    if (present(shape))sample%shape=shape
    if (present(x_geom))sample%x_geom=x_geom
    if (present(y_geom))sample%y_geom=y_geom
    if (present(position))sample%position=position
    if (present(dimensions))sample%dimensions=dimensions
    if (present(radius))sample%radius=radius
    if (present(inner_radius))sample%inner_radius=inner_radius
    if (present(height))sample%height=height
    if (present(mass))sample%mass=mass
    if (present(molecular_weight))sample%molecular_weight=molecular_weight
    if (present(xcoh))sample%xcoh=xcoh
    if (present(xinc))sample%xinc=xinc
    if (present(xabs))sample%xabs=xabs

    call IXFcheck(sample,status) 

  end subroutine IXFset_sample

  !-----------------------------------------------------------------------------------------------------------------------
  !! The IXFget subroutine will return elements of an object to an optional supplied arrays/variables. The supplied variables
  !! should be referrred to by keyword to avoid errors. The 'wout' variable is special and can be used to copy the 
  !! contents of a whole object to a new one.

  subroutine IXFget_sample(sample,status,name,chemical_formula,temperature,electric_field,electric_coord,  &
       magnetic_field,magnetic_coord,pressure,lattice,uvec,vvec,psi,omega,gonio,shape,x_geom,y_geom,position,dimensions, &
       radius,inner_radius,height,mass,molecular_weight,xcoh,xinc,xabs,wout)

    implicit none
    type(IXTsample),intent(inout)::sample
    type(IXTsample),optional,intent(out)::wout
    type(IXTlattice),optional,intent(out)::lattice
    character(len=*),optional,intent(out)::name,chemical_formula
    character(len=*),optional,intent(out)::electric_coord,magnetic_coord,shape
    real(dp),optional,intent(out)::temperature,electric_field(3),magnetic_field(3),pressure,uvec(3),vvec(3),psi,omega,   &
         gonio(3),x_geom(3),y_geom(3),position(3),dimensions(3),radius,inner_radius,height,mass,molecular_weight, &
         xcoh,xinc,xabs
    type(IXTstatus)::status

    if (present(wout))call IXFcopy(sample,wout,status)
    if (present(name))name=sample%name
    if (present(chemical_formula))chemical_formula=sample%chemical_formula
    if (present(temperature))temperature=sample%temperature  
    if (present(electric_field))electric_field=sample%electric_field
    if (present(electric_coord))electric_coord=sample%electric_coord
    if (present(magnetic_field))magnetic_field=sample%magnetic_field
    if (present(magnetic_coord))magnetic_coord=sample%magnetic_coord
    if (present(pressure))pressure=sample%pressure
    if (present(lattice))call IXFcopy(sample%lattice,lattice,status)
    if (present(uvec))uvec=sample%uvec
    if (present(vvec))vvec=sample%vvec
    if (present(psi))psi=sample%psi
    if (present(omega))omega=sample%omega
    if (present(gonio))gonio=sample%gonio
    if (present(shape))shape=sample%shape
    if (present(x_geom))x_geom=sample%x_geom
    if (present(y_geom))y_geom=sample%y_geom
    if (present(position))position=sample%position
    if (present(dimensions))dimensions=sample%dimensions
    if (present(radius))radius=sample%radius
    if (present(inner_radius))inner_radius=sample%inner_radius
    if (present(height))height=sample%height
    if (present(mass))mass=sample%mass
    if (present(molecular_weight))molecular_weight=sample%molecular_weight
    if (present(xcoh))xcoh=sample%xcoh
    if (present(xinc))xinc=sample%xinc
    if (present(xabs))xabs=sample%xabs

    call IXFcheck(sample,status) 

  end subroutine IXFget_sample
  !-----------------------------------------------------------------------------------------------------------------------
  !!IXFdestroy routine deallocates any pointer arrays in the type, and calls the destroy function on any nested
  !!objects, could also be used to set variables back to their default values.
  !!If one of the objects is an array of structures, then each structure will be recursively destroyed by the
  !!IXFdealloc function.

  subroutine IXFdestroy_sample(sample,status)
    implicit none
    type(IXTsample)::sample
    type(IXTstatus)::status

    call IXFdestroy(sample%base,status)

    if(IXFvalid(sample%lattice))call IXFdestroy(sample%lattice,status)

    ! the initialised status is now revoked for the object
    ! this statement MUST exist in all destroy routines
    call IXFclear_valid(sample)

  end subroutine IXFdestroy_sample

end module IXMsample
