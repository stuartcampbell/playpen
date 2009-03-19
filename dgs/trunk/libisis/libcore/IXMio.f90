! Comments will be formatted by F90DOC - see http://theory.lcs.mit.edu/~edemaine/f90doc/ for syntax
!-------------------------------------------------------------------------------
!! Write a line of text to the default standard output device
!! This calls the internal IXBwriteline function which,
!! If external (e.g. MATLAB) bindings are being used, will be 
!! replaced by an equivalent one from the appropriate bindings library
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 503 $ ($Date: 2005-10-24 14:04:40 -0400 (Mon, 24 Oct 2005) $)
module IXMio

contains
	subroutine IXFwrite_line(line,status)
	  use IXMtype_definitions
	  use IXMstatus
	  implicit none
	  external IXBwrite_line !! bindings dependent screen output function
	  character(len=*) line !! text to write to the output device (screen)
	  type(IXTstatus) :: status
!open(666,file='display.out',access='append')
!write(666,*) line
!close(666)
	  call IXBwrite_line(line,status)
	end subroutine IXFwrite_line

!! write a line of text indented by "indent" characters using "ch" to
!! fill the space up. Not that "indent" refers to the size of the indent and
!! not the number of occurrences of "ch" in the output - "ch" can be several characters
!! long and the final occurrence will be truncated as necessary to fit a size of "indent"
!!
!! @author Freddie Akeroyd, ISIS
!! @version $Revision: 503 $ ($Date: 2005-10-24 14:04:40 -0400 (Mon, 24 Oct 2005) $)
	subroutine IXFwrite_line_indent(line,indent,ch,status)
	  use IXMtype_definitions
      use IXMstatus
	  implicit none
	  character(len=512) buffer
	  character(len=*) line		!! line of text to write
	  character(len=*) ch		!! indenting character string to use
	  integer :: indent			!! number of characters to indent by
	  integer :: lench, i, j, ind
	  type(IXTstatus) :: status
          buffer = ' '
	  if (indent > 0) then
        lench = len(ch)
        ind = min(indent,len(buffer))
		do i = 0, ind-1
			j = i*lench + 1
			buffer(j:j+lench-1) = ch
		enddo
                j = ind * lench
		do i=1,min(len(line),len(buffer)-j)
		    buffer(i+j:i+j) = line(i:i)
		enddo
		call IXFwrite_line(buffer, status)
	  else
		call IXFwrite_line(line, status)
	  endif
	end subroutine IXFwrite_line_indent
end module IXMio

! special one used only by status object
! this is needed because IXMio includes status so IXMstatus cannot include IXMio
subroutine IXIwrite_line(line,status)
    use IXMio
	  use IXMtype_definitions
	  use IXMstatus
	  implicit none
	  character(len=*) line !! text to write to the output device (screen)
	  type(IXTstatus) :: status
	  call IXFwrite_line(line,status)
end subroutine IXIwrite_line
