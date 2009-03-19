#include "fintrf.h"

program main
    implicit none
    mwpointer, external :: engOpen
    mwpointer :: ep
    integer, external :: engEvalString, engClose
    integer :: status
    character(len=132) :: line

    ep = engOpen('matlab ')
    if (ep .eq. 0) then
       write(6,*) 'Can''t start MATLAB engine'
       stop
    endif
     
    print *, 'Enter MATLAB commands'

    do while(.true.)
        read(*,*,end=20,err=20) line
        if (engEvalString(ep, line) .ne. 0) then
             write(6,*) 'engEvalString failed'
        endif
    end do
 20 continue

    status = engClose(ep)
    if (status .ne. 0) then 
       write(6,*) 'engClose failed'
    endif
end
