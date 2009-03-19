program make_xml
use IXMlibcore
use IXMrunfile
implicit none
type(IXTstatus) :: status
type(IXTfileio) :: fio
type(IXTmoderator) :: moderator
type(IXTsource) :: source
type(IXTrunfile) :: runfile
type(IXTfermi_chopper) :: fermi_chopper
write(6,*) 'Starting program make_xml'
if (IXFlibrary_init() /= 0) then
    stop 'error initialising library'
endif

#define IXD_NAME	moderator
#define IXD_CNAME	'moderator'
#include "xml_routines.f90"

#define IXD_NAME	fermi_chopper
#define IXD_CNAME	'fermi_chopper'
#include "xml_routines.f90"

#define IXD_NAME	runfile
#define IXD_CNAME	'runfile'
#include "xml_routines.f90"

#define IXD_NAME	source
#define IXD_CNAME	'source'
#include "xml_routines.f90"

call IXFlibrary_finish(status)

end
