!-----------------------------------------------------------------------------------------------------------------------------------
! MODULE: IXMtools
!-----------------------------------------------------------------------------------------------------------------------------------
!! @author Toby Perring, ISIS
!! @version $Revision: 1407 $ ($Date: 2008-06-27 07:21:57 -0400 (Fri, 27 Jun 2008) $)
!!
!! Fortran utilities for parsing character strings, input/output etc. These are minimal Fortran 90 conversions of the
!! original F77 tools (T.G.Perring, the core of which was based on routines of Ray Osborn).

module IXMtools
   use IXMtype_definitions
   use IXMstatus
   use IXMio
   implicit none
!   private
   public ::  loc_ch,getlin, getlf, inext, inxtch, iprvch, lenstr,itoc,icmd,newfl,oldfl,ctoc,getwrd
   public :: ctoxi, ctoxi2, ctoi, ctoi2, geti, getis, ctoxd, ctod, getd, getds, ctoxr, ctor, getr, getrs
   public ::  remark, prompt, homer_message, read_line, upcase, locase, cspace,getstr,putlf,lalfch,lnamch
   
   integer(i4b), parameter :: stdin=-5_i4b, stdout=-6_i4b
   integer(i4b), parameter :: ok=0_i4b, warn=-1_i4b, err=-100_i4b, eof=-101_i4b
   integer(i4b), parameter :: read=1001_i4b, readwr=1002_i4b
   integer(i4b), parameter :: old=1101_i4b, new=1102_i4b, oldnew=1103_i4b
   character(len=4), parameter :: trail_space=achar(9)//achar(32)//achar(0)//achar(13)       !! tab, space, null, <CR>
   character(len=2), parameter :: numeric_list_terminator = ',;'
contains

   !----------------------------------------------------------------------------------------------------------------------
   !  Writing strings to the screen or to file
   !----------------------------------------------------------------------------------------------------------------------
   !! Prints a character string to the screen:
   !!v   call remark (string)

   subroutine remark (string)
      character(len=*), intent(IN) :: string
      integer(i4b) :: l
      type(IXTstatus) :: status

      l = lenstr(string)
      if (l > 0) then
         call IXFwrite_line (string(1:l), status)
      endif

      return
   end subroutine remark

   !----------------------------------------------------------------------------------------------------------------------
   !! Prints a character string to the screen without carriage return. Suitable for printing prompts for user input:
   !!v   call prompt (string)
   !! The prompt is the characters in string, without any other characters such as '> ' appended.

   subroutine prompt (string)
      character(len=*), intent(IN) :: string
      integer(i4b) :: l
      type(IXTstatus) :: status

      l = lenstr(string)
      if (l > 0) then
         write (*,'(1x,a,1x)',advance='NO') string(1:l)        ! *** not general enough, needs IXMio to be enhanced
         !        call IXFwrite_prompt (string(1:l), status)   ! *** doesn't yet exist (25/10/05)
      endif

      return
   end subroutine prompt

   !----------------------------------------------------------------------------------------------------------------------
   !! Compose and write a message in a standard form to the screen:
   !!v   call homer_message (intro, message, flname)
   !! results in the output:
   !!>   intro: message
   !!<          flname

   subroutine homer_message (intro, message, flname)
      character(len=*), intent(IN) :: intro
      character(len=*), intent(IN) :: message
      character(len=*), intent(IN) :: flname
      integer:: l_i, l_m, l_f
      character(len=255):: line
      type(IXTstatus) :: status

      l_i = lenstr(intro)
      l_m = lenstr(message)
      l_f = lenstr(flname)

      line = intro(1:l_i)//': '//message(1:l_m)
      call IXFwrite_line (line, status)
      if (l_f /= 0) then
         call IXFwrite_line_indent(line, 6, ' ', status)
      endif

      return
   end subroutine homer_message

   !----------------------------------------------------------------------------------------------------------------------
   !! Read a line from the screen, returning the length stripped of trailing space
   
   integer(i4b) function getlin (line)
      character(len=*), intent(out) :: line
      getlin = getlf (STDIN, line)
      return
   end function getlin

   !----------------------------------------------------------------------------------------------------------------------
   !   integer function GETLIN (line)
   !
   !  Inputs line from the default input stream, returning its trimmed length.
   !
   !  If journalling is on, then writes the line to the journal file
   !
   !  Note that lines beginning with an exclamation mark (!) are ignored
   ! if the program is running a batch job. Jounalling is inactive while a
   ! batch file is running.
   !
   !  If journalling is on, then writes to the journal file
   !
   ! Arguments
   ! ---------
   !   ch*(*)     LINE  character variable
   !              i/p:  irrelevant
   !              o/p:  line read from input stream.
   !                   if LINE is longer than the record read, then
   !                        LINE is padded by blanks
   !                   If the record is longer than LINE, then the
   !                        first LEN(LINE) characters are read
   !
   ! Function
   ! --------
   !   integer  GETLIN
   !              i/p:  -
   !              o/p:  trimmed length of LINE
   !
   ! Subroutine calls
   ! ----------------
   !   GETLF
   !
   !
   !     integer(i4b) function GETLIN (line)
   !     character*(*) line
   !C
   !      include 'tools.par'
   !  include 'tools_batch.inc'
   !
   !      integer i, ibeg, istatus, GETLF, PUTLF, INEXT
   !  character*255 string, file_name
   !      external GETLF, PUTLF, INEXT
   !
   !C
   !C Read line from default input string or batch file
   ! 100 if (b_level .eq. 0) then
   !     GETLIN = GETLF (STDIN, line)
   !     call journal_write (line)
   !  else
   !     GETLIN = GETLF (b_unit, line)
   !     if (GETLIN .ge. 0) then       ! read line from file
   !        if (GETLIN .gt. 0) then
   !           i = 1
   !           ibeg = INEXT(line, i)
   !           if (line(ibeg:ibeg) .eq. '!') goto 100
   !        endif
   !        istatus = PUTLF (STDOUT, line)
   !        return
   !     else if (GETLIN .eq. EOF) then   ! reached end of file
   !        close (b_unit)             ! close file
   !        b_unit_arr(b_level) = 0       ! cleanup array element
   !        b_level = b_level - 1         ! reduce nesting level
   !        if (b_level .gt. 0) b_unit = b_unit_arr(b_level)   ! recover unit number for nesting level
   !        goto 100
   !     else                    ! error condition
   !        inquire (unit=b_unit, name=file_name)
   !        string = 'ERROR reading from batch file :'//file_name
   !        call batch_close
   !        goto 100
   !     endif
   !  endif
   !
   !      return
   !      end

   !----------------------------------------------------------------------------------------------------------------------
   !!  Reads a line from a given unit number stream, returning its trimmed length
   !!v   lstr = getlf (iunit, line)
   !!>   input:   iunit    unit number
   !!    output:  line     line of text read from file (padded with trailing blanks if necessary)
   !!             getlf    if +ve: trimmed length of the line read from file
   !!                      if -ve: problem reading:
   !!                         = EOF if end of file
   !!<                        = ERR if any other error

   integer(i4b) function getlf (iunit, line)
      integer(i4b), intent(IN) :: iunit
      character(len=*), intent(OUT) :: line
      integer(i4b) :: i

      if (iunit == STDIN) then
         read (*, fmt='(A)', end=100, err=200) line      ! *** not general enough, needs IXMio to be enhanced
      else
         read (iunit, fmt='(A)', end=100, err=200) line  ! *** not general enough, needs IXMio to be enhanced
      endif
      getlf = lenstr(line)

      return

      ! end of file reached:
100   if (iunit == STDIN) then
         getlf = 0
      else
         getlf = EOF
      endif
      return

      ! error reading from file:
200   if (iunit == STDIN) then
         getlf = 0
      else
         getlf = ERR
      endif
      return

   end function getlf
   
   !----------------------------------------------------------------------------------------------------------------------
   !! Reads a line from the file, skipping over blank lines and those beginning with an exclamation mark
   !!v   call read_line (iunit, line, len_line)
   !!
   !!>   input:   iunit    unit number
   !!    output:  line     line of text read from file (set to blanks if nothing read)
   !!             len_line If +ve: trimmed length of line read from file
   !!                      if -ve: problem reading:
   !!                         = EOF if end of file
   !!<                        = ERR if any other error

   subroutine read_line (iunit, line, len_line)
      integer(i4b):: iunit
      character(len=*):: line
      integer(i4b):: len_line, i, ibeg

1000  len_line = getlf (iunit, line)
      if (len_line > 0) then        ! characters read from line
         i = 1
         ibeg = inext(line,i)
         if (line(ibeg:ibeg) == '!') then
            goto 1000
         else
            return
         endif
      else if (len_line == 0) then  ! no characters read - try again
         goto 1000
      else if (len_line == ERR) then 
         call cspace (line)
         return
      else if (len_line == EOF) then
         call cspace (line)
         return
      else
         call remark (' *** UNKNOWN ERROR - CONTACT T.G.PERRING')
         stop
      endif

   end subroutine read_line

   !----------------------------------------------------------------------------------------------------------------------
   !  String inquiry and transformation routines
   !----------------------------------------------------------------------------------------------------------------------
   !! Finds the position of the next non-trailing space character
   !! in a string, starting from (and including) the ith position
   !!v  integer function INEXT (string, i)
   !!
   !!>  Input:  string  Character variable or constant
   !!                i  Integer giving position of current location in the string
   !!                   If i is -ve, it is interpreted internally as +ve 
   !!
   !!   ouput:   inext  Position of next non-trailing space (tab, space, null or <CR>)
   !!                   If not found, then the pointer is set to len(string)+1
   !!<
   integer(i4b) function inext (string, i)
      character(len=*), intent(in) :: string
      integer(i4b), intent(in) :: i
      integer(i4b) :: ibeg, j

      ibeg = max(1,abs(i))
      if (ibeg > len(string)) then
         inext = len(string) + 1
         return
      endif
      
      do j = ibeg, len(string)
         if (index(trail_space, string(j:j)) == 0) then
            inext = j
            return
         endif
      end do
      inext = len(string) + 1

      return
   end function inext

   !----------------------------------------------------------------------------------------------------------------------
   !! Finds the location in a character string of the next occurence of one of the characters in a substring,
   !! starting from (and including) position i:
   !!v   location = inxtch (string, i, ch)
   !!
   !!>  Input:  string  Character variable or constant
   !!                i  Integer giving position of current location in the string
   !!                   If i is -ve, it is interpreted internally as +ve
   !!               ch  Character substring containing list of characters to search for 
   !!
   !!   ouput:  inxtch  /= 0 : position of next occurence of a character in ch
   !!                   == 0 : not found
   !!<
   integer(i4b) function inxtch (string, i, ch)
      character(len=*), intent(IN) :: string
      integer(i4b), intent(IN) :: i
      character(len=1), intent(IN) ::ch
      integer(i4b):: ibeg, j

      inxtch = 0  ! default return value

      ibeg = max(1,abs(i))
      if (ibeg > len(string)) return
      
      if (len(ch) == 1) then    ! this might be faster if a single character
         do j = ibeg, len(string)
            if (string(j:j) == ch) then
               inxtch = j
               return
            endif
         end do
      else
         do j = ibeg, len(string)
            if (index(ch,string(j:j)) /= 0) then
               inxtch = j
               return
            endif
         end do
      endif

      return
   end function inxtch

   !----------------------------------------------------------------------------------------------------------------------
   !! Finds the location in a character string of the previous occurence of one of the characters in a substring,
   !! starting from (and including) position i:
   !!v      location = iprvch (string, i, ch)
   !!
   !!>  Input:  string  Character variable or constant
   !!                i  Integer giving position of current location in the string
   !!                   If i is -ve, it is interpreted internally as +ve
   !!               ch  Character substring containing list of characters to search for 
   !!
   !!   ouput:  inxtch  /= 0 : position of previous occurence of a character in ch
   !!                   == 0 : not found
   !!<
   integer(i4b) function iprvch (string, i, ch)
      character(len=*), intent(IN) :: string
      integer(i4b), intent(IN) :: i
      character(len=1), intent(IN) ::ch
      integer(i4b):: ibeg, j

      iprvch = 0  ! default return value

      ibeg = max(1,abs(i))
      if (ibeg > len(string)) return
      
      if (len(ch) == 1) then
         do j = ibeg, 1, -1
            if (string(j:j) == ch) then
               iprvch = j
               return
            endif
         end do
      else
         do j = ibeg, 1, -1
            if (index(ch,string(j:j)) /= 0) then
               iprvch = j
               return
            endif
         end do
      endif

      return
   end function iprvch



   !----------------------------------------------------------------------------------------------------------------------
   !! Finds the first location of a specific character(s) in a character string 
   !! starting from (and including) position i:
   !!v      location = iprvch (string, i, ch)
   !!
   !!>  Input:  string  Character variable or constant
   !!                i  Integer giving position of current location in the string
   !!                   If i is -ve, it is interpreted internally as +ve
   !!               ch  Character substring containing list of characters to search for 
   !!
   !!   ouput:  inxtch  /= 0 : position of previous occurence of a character in ch
   !!                   == 0 : not found
   !!<
   integer(i4b) function loc_ch (string, i, ch)
      character(len=*), intent(IN) :: string
      integer(i4b), intent(IN) :: i
      character(len=*), intent(IN) ::ch
      integer(i4b):: ibeg, j

      loc_ch = 0  ! default return value

      ibeg = max(1,abs(i))
      if (ibeg > len(string)) return
      
         do j = ibeg, len(string)-len(ch)+1
            if (string(j:j-1+len(ch)) == ch) then
               loc_ch = j
               return
            endif
         end do

      return
   end function loc_ch



   !----------------------------------------------------------------------------------------------------------------------
   !! Place the pointer immediately after a deliminating character from a given list.
   !!
   !! The purpose of a routine is to determine if the characters up to the present pointer position are separated 
   !! from the remainder of the string, either by one of the deliminating characters or by trailing space. If it is,
   !! the return value is set to the position immediately after the deliminating character, or at the start of the
   !! trailing space respectively (that is, |i|). If there is no separation, the return value is set to -|i|.
   !!
   !!v      location = idelim (string, i, ch)
   !!
   !!>  Input:  string  Character variable or constant
   !!                i  Integer giving position of current location in the string
   !!                   If i is -ve, it is interpreted internally as +ve
   !!               ch  Character substring containing list of terminator characters to search for 
   !!
   !!   ouput:  idelim  +ve : position immediately following a deliminator character
   !!                   -ve : no delimitation
   !!<
   integer(i4b) function idelim (string, i, ch)
      character(len=*), intent(IN) :: string
      integer(i4b), intent(IN) :: i
      character(len=*), intent(IN) ::ch
      integer(i4b):: ibeg, j
      
      ibeg = max(1,abs(i))
      if (ibeg > len(string)) then
         idelim = len(string) + 1      
         return
      endif
      
      idelim = inext(string,ibeg)
      if (idelim <= len(string)) then
         if (index(ch,string(idelim:idelim)) /= 0) then
            idelim = idelim + 1
         else 
            if (idelim == ibeg) then
               idelim = -ibeg
            else
               idelim = ibeg
            endif
         endif
      else
         idelim = ibeg
      endif
      
      return
   end function idelim

   !----------------------------------------------------------------------------------------------------------------------
   !! Finds the length of the input string STRING, excluding trailing space (blanks and tabs, carriage return and 
   !! null character).
   !!v      lstr = lenstr (string)
   !!
   !!>   input:   string   Character constant or variable
   !!    output:  lenstr   Length of trimmed string; 0 =< lenstr =< len(string)
   !!<
   integer(i4b) function lenstr (string) 
      character(len=*), intent(IN) :: string
      integer(i4b) :: j

      lenstr = len(string)
      do j = len(string), 1, -1
         if (index(trail_space, string(j:j)) /= 0) then
            lenstr = lenstr - 1
         else
            return
         endif
      end do

      return
   end function lenstr

   !----------------------------------------------------------------------------------------------------------------------
   !! Raise the case of a character string
   subroutine upcase (string)
      character(len=*), intent(INOUT) :: string
      integer(i4b) :: i

      do i = 1, len(string)
         if (LGE(string(i:i),'a') .and. LLE(string(i:i),'z')) then
            string(i:i) = achar(iachar(string(i:i)) - 32)
         endif
      end do

      return
   end subroutine upcase

   !----------------------------------------------------------------------------------------------------------------------
   !! Lower the case of a character string
   subroutine locase (string)
      character(len=*), intent(INOUT) :: string
      integer(i4b) :: i

      do i = 1, len(string)
         if (LGE(string(i:i),'A') .and. LLE(string(i:i),'Z')) then
            string(i:i) = achar(iachar(string(i:i)) + 32)
         endif
      end do

      return
   end subroutine locase

   !----------------------------------------------------------------------------------------------------------------------
   !! Fills a string with spaces. Provided because it is not entirely clear in the Fortran90 standard that the
   !! assignment my_string = ' ' results in all characters in my_string being set to ' '. 
   !!v      call cspace(string)
   
   subroutine cspace (string)
      integer(i4b) :: i
      character(len=*) :: string

      do i = 1, len(string)
         string(i:i) = ' '
      end do

      return
   end subroutine cspace

   !----------------------------------------------------------------------------------------------------------------------
   !  Reading integers from a character string
   !----------------------------------------------------------------------------------------------------------------------
   !! Starts at the current position in the string and reads in characters until the string is terminated by a
   !! character that is inconsistent with interpretation of the string as an integer.
   !!
   !!v      value = ctoxi (string, i)
   !!
   !!>   input:   string   Character string
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: If +ve: integer was read; i is set to position of character immediately after the 
   !!                           integer.
   !!                           If -ve: no integer read; abs(i) is the position of the character that caused the
   !!                           problem. This includes if the end of the line was reached without reading an integer.
   !!    output:   ctoxi   Value of integer read from string. If there was a problem, ctoxi = 0.
   !!<

   integer(i4b) function ctoxi (string, i)
      character(len=10), parameter :: digits='0123456789'
      character(len=*), intent(IN) :: string
      integer(i4b), intent(INOUT) :: i
      integer(i4b) :: ibeg, d
      logical :: minus

      ctoxi = 0
      
      ! Strip away leading blanks
      i = inext (string,i)
      if (i > len(string)) then; i = -i; return; endif
      
      ! Determine sign of putative integer:
      if (string(i:i) == '-') then
         i = i + 1
         if (i > len(string)) then; i = -i; return; endif
         minus = .TRUE.
      else if (string(i:i) == '+') then
         i = i + 1
         if (i > len(string)) then; i = -i; return; endif
         minus = .FALSE.
      else
         minus = .FALSE.
      endif

      ! Now get digits:
      ibeg = i
20    continue
      d = index (digits, string(i:i))
      if (d /= 0) then
         ctoxi = 10*ctoxi + (d-1)
         i = i + 1
         if (i > len(string)) then
            if (minus) ctoxi = -ctoxi
            return
         endif
         goto 20
      else
         if (i /= ibeg) then
            if (minus) ctoxi = -ctoxi
            return
         else
            i = -i
            return
         endif
      endif

   end function ctoxi

   !----------------------------------------------------------------------------------------------------------------------
   !! Reads an integer, or if possible two integers that define a range of values. Valid
   !! forms for range are i-j or i>j, where i &/or j can be negative, and the range characters - and > can 
   !! be separated with white space from i,j.
   !!
   !!v      n = ctoxi2 (string, i, value_1, value_2)
   !!
   !!>   input:   string   Character string
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: If +ve: 1 or 2 integers were read; i is set to position of character immediately after the 
   !!                           integer(s).
   !!                           If -ve: no integer read; abs(i) is the position of the character that caused the
   !!                           problem (including if the end of the line was reached without reading an integer).
   !!            value_1   Start of range (ctoxi2 = 1 or 2; unchanged otherwise)
   !!            value_2   End of range (ctoxi2 = 2; unchanged otherwise)
   !!    output:  ctoxi2   Number of integers read from string (0,1,or 2)
   !!<

   integer(i4b) function ctoxi2 (string, i, k1, k2)
      character(len=*), intent(in) :: string
      integer(i4b), intent(inout)  :: i
      integer(i4b), intent(inout)  :: k1, k2
      integer(i4b) :: iprime, ich, k
      character(len=1) :: ch
      logical:: kneg

      ctoxi2 = 0; k1 = 0; k2 = 0

      ! Get first integer
      k = ctoxi (string,i)
      if (i > 0) then                  ! Succesfully read first integer
         ctoxi2 = 1
         k1 = k
         if (i > len(string)) return   ! Return if reached end of the line
      else
         return
      endif

      ! Get next character
      ich = inext(string,i)
      if (ich > len(string)) return    ! No next character; return with +ve i and ctoxi2=1
      ch = string(ich:ich)
      if ((ch /= '-') .and. (ch /= '>')) return ! next character is not a range indicator, so return

      ! Get second integer
      iprime = ich+1
      if (iprime > len(string)) return
      if ((ich /= i) .and. (ch == '-') .and. (inext(string,iprime) == iprime)) then
         kneg = .TRUE.
      else
         kneg = .FALSE.
      endif
      
      k = ctoxi(string,iprime)
      if ((iprime < 0) .or. (kneg .and. (k >= 0))) then  ! inconsistent format
         return
      else
         i = iprime
         ctoxi2 = 2
         k2 = k
         return
      endif
      return

   end function ctoxi2

   !----------------------------------------------------------------------------------------------------------------------
   !! Reads an integer from a character string, starting at a given location. The integer needs to be delimited
   !! from the rest of the string.
   !!
   !! The pointer i is placed after the delimiting character if an integer is read; if an integer is not read
   !! then abs(i)=position of offending character (compare geti).
   !!
   !!v      value = ctoi (string, i)
   !!
   !!>   input:   string   Character string
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: If +ve: integer was read; i is set to position of character immediately after the 
   !!                                  delimiting character (end of string, trailing space or comma).
   !!                           If -ve: no integer read; abs(i) is the position of the character that caused the
   !!                                  problem (e.g. invalid delimiting character, or end of line reached).
   !!    output:    ctoi   Value of integer read from string. If there was a problem, ctoi = 0.
   !!<

   integer(i4b) function ctoi (string, i)
      character(len=*), intent(in) :: string
      integer(i4b), intent(inout) :: i

      ctoi = ctoxi(string,i)  
      if (i>0) then  ! successfully read undelimited integer; now check if delimited
         i = idelim(string, i, NUMERIC_LIST_TERMINATOR)
         if (i<0) then
            ctoi = 0
         endif
      endif

      return
   end function ctoi
   
   !----------------------------------------------------------------------------------------------------------------------
   !! Reads an integer, or if possible two integers that define a range of values. Valid
   !! forms for range are i-j or i>j, where i &/or j can be negative (and the range characters - and > can 
   !! be separated with white space from i,j). The integer(s) must be delimited from the remainder of the string.
   !!
   !! The pointer i is placed after the delimiting character (or at the start of delimiting trailing space)
   !! if integer(s) read; if integer(s) not read then abs(i)=position of offending character.
   !!
   !!v      n = ctoi2 (string, i, value_1, value_2)
   !!
   !!>   input:   string   Character string
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: If +ve: 1 or 2 integers were read; i is set to position of character immediately after the 
   !!                           the delimiting character, or at the start of delimiting trailing space.
   !!                           If -ve: no integer read; abs(i) is the position of the character that caused the
   !!                           problem (e.g. invalid delimiting character, or end of line reached).
   !!            value_1   Start of range (ctoi2 = 1 or 2; unchanged otherwise)
   !!            value_2   End of range (ctoi2 = 2; unchanged otherwise)
   !!    output:   ctoi2   Number of integers read from string (0,1,or 2)
   !!<
   integer(i4b) function ctoi2 (string, i, k1, k2)
      character(len=*), intent(in) :: string
      integer(i4b), intent(inout) :: i, k1, k2
      integer(i4b) :: k1_temp, k2_temp

      ctoi2 = ctoxi2 (string, i, k1_temp, k2_temp)
      if (i>0) then  ! successfully read undelimited integer(s); now check if delimited
         i = idelim(string, i, NUMERIC_LIST_TERMINATOR)
         if (i<0) then
            ctoi2 = 0
         else
            k1 = k1_temp
            k2 = k2_temp
         endif
      endif

      return
   end function ctoi2

   !----------------------------------------------------------------------------------------------------------------------
   !! Reads an integer from a character string, starting at a given location. The integer needs to be delimited
   !! from the rest of the string. The pointer i is placed after the delimiting character (or at the start of
   !! delimiting trailing space).
   !!
   !! Almost equivalent to calling getis (string, i, value, m [,iproblem]) with m=1. The difference is that getis
   !! will take only as many values from a range n1-n2 as required to fill the input array; the rest are discarded.
   !! Thus, for example, nval=getis('12-15 people',i,val,1,iprob), i=1 will set val=12, i=6, iprob=0 and nval=1,
   !! whereas nval=getis('12-15 people',i,val,iprob) will leave val unchanged from input, i=1, iprob=3 and nval=0
   !! because the single integer, 12, is not properly delimited.
   !!
   !!v      n = geti (string, i, value [,iproblem])
   !!
   !!>   input:   string   Character string
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: Position immediately after the delimiting character (or at the start of delimiting
   !!                           trailing space) following succesfully read integer.
   !!                           If the end of the line was reached without reading an integer, then i = len(string)+1.
   !!              value   Value read from string (unchanged if problem)
   !!           iproblem   If integer read (geti=1): set to zero
   !!                      If was not read (geti=0): set to position of problem character
   !!    output:    geti   Number of integers read from string (0 or 1).
   !!<
   ! *** Consider making a call to getis with m=1 to guarantee the equivalence of the two functions

   integer(i4b) function geti (string, i, value, iproblem)
      character(len=*), intent(in):: string
      integer(i4b), intent(inout) :: i, value
      integer(i4b), intent(inout), optional :: iproblem
      integer(i4b) :: itemp, vtemp

      i = max(1,abs(i))
      itemp = i
      vtemp = ctoi (string, itemp)
      if (itemp > 0) then  ! integer read
         i = itemp
         value = vtemp
         if (present(iproblem)) iproblem = 0
         geti = 1
      else
         if (abs(itemp) > len(string)) then  ! reached end of line without reading integer
            i = len(string) + 1
            if (present(iproblem)) iproblem = 0
         else
            if (present(iproblem)) iproblem = -itemp
         endif
         geti = 0
      endif
      return

   end function geti

   !----------------------------------------------------------------------------------------------------------------------
   !! Reads a list of integers from a string, upto a specified maximum number of values. The integers must all be
   !! properly delimited from each other, and the final integer delimited from the remainder of the string.
   !! The pointer i is placed after the delimiting character (or at the start of delimiting trailing space).
   !!
   !! If non-delimited integers were encountered, then getis gives the number of delimited integers read up until that
   !! point, i is placed after the delimitor past the last integer read (or at the start of delimiting trailing space),
   !! and the (optional) argument iproblem the location of the offending character.
   !!
   !!v      nvals = getis (string, i, values, m [,iproblem])
   !!
   !!>   input:   string   Character string
   !!                  m   Maximum number of integers to be read
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: Position immediately after the delimiting character (or at the start
   !!                          of delimiting trailing space), following the last integer succesfully read.
   !!                           If the end of the line was reached without reading m integers, then i = len(string)+1.
   !!             values   Integer array whose first getis (below) values are filled from the string
   !!           iproblem   =0: No undelimited integers encountered (i.e. no syntactical problems)
   !!                      >0: Location of offending character if undelimited integer encountered.
   !!    output:   getis   Number of integers read from the string.
   !!<
   integer(i4b) function getis (string, i, values, m, iproblem)
      character(len=*), intent(in) :: string
      integer(i4b), intent(inout) :: i, values(:)
      integer(i4b), intent(in) :: m
      integer(i4b), intent(inout), optional :: iproblem
      integer(i4b) :: m_max, isave, n, k, k1, k2, nvals_in_range, nelements_left, sgn

      i = max(1,abs(i))
      m_max = min(size(values),m)
      if (present(iproblem)) iproblem = 0
      getis = 0
      if (m < 1) return ! dummy function if no integers asked for
      do while (getis < m_max)
         isave = i   ! keep in case error in ctoi2
         n = ctoi2 (string, i, k1, k2)
         if (n == 2) then
            nvals_in_range = abs(k2-k1) + 1
            nelements_left = m_max - getis
            sgn = sign(1_i4b,k2-k1)
            do k = k1, k1 + sgn*(min(nvals_in_range,nelements_left)-1), sgn
               getis = getis + 1
               values(getis) = k
            end do
         elseif (n == 1) then
            getis = getis + 1
            values(getis) = k1
         elseif (n == 0) then    ! Error reading delimited integers
            if (abs(i) > len(string)) then   ! problem was simply because end of line was reached; this is OK
               i = len(string) + 1
            else
               if (present(iproblem)) iproblem = -i
               i = isave
            endif
            return
         endif
      end do
      
      return
   end function getis

   !----------------------------------------------------------------------------------------------------------------------
   !  Reading reals from a character string
   !----------------------------------------------------------------------------------------------------------------------
   !! Starts at the current position in the string and reads in characters until the string is terminated by a
   !! character that is inconsistent with interpretation of the string as a real.
   !!
   !!v      value = ctoxd (string, i)
   !!
   !!>   input:   string   Character string
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: If +ve: real was read; i is set to position of character immediately after the 
   !!                           integer.
   !!                           If -ve: no real read; abs(i) is the position of the character that caused the
   !!                           problem. This includes if the end of the line was reached without reading an real.
   !!    output:   ctoxd   Value of real read from string. If there was a problem, ctoxd = 0.
   !!<
   real(dp) function ctoxd (string, i)
      character(len=10), parameter :: digits='01234567890'
      character(len=*), intent(in) :: string
      integer(i4b), intent(inout) :: i
      integer(i4b) ibeg, d, iexp, ipwr, iminus
      real(dp) factor
      logical minus, point, mant

      ctoxd = 0.0_dp
      
      ! Strip away leading blanks
      i = inext (string,i)
      if (i > len(string)) then; i = -i; return; endif
      
      ! Determine sign of putative real:
      if (string(i:i) == '-') then
         i = i + 1
         if (i > len(string)) then; i = -i; return; endif
         minus = .TRUE.
      else if (string(i:i) == '+') then
         i = i + 1
         if (i > len(string)) then; i = -i; return; endif
         minus = .FALSE.
      else
         minus = .FALSE.
      endif

      ! Now get digits
      ibeg = i
      mant = .FALSE.
      point= .FALSE.
      factor = 1.0_dp
      iexp = 0
20    continue
      d = index (digits, string(i:i))
      if (d /= 0) then
         mant = .TRUE.
         ctoxd = ctoxd + factor*real(d-1,dp)
         if (.not. point) iexp = iexp + 1
         factor = factor / 10.0_dp
         i = i + 1
         if (i > len(string)) then
            ctoxd = ctoxd * 10.0_dp**(iexp-1)
            if (minus) ctoxd = -ctoxd
            return
         endif
         goto 20
      else if ((string(i:i) == '.') .and. (.not. point)) then
         point = .TRUE.
         i = i + 1
         if (i > len(string)) then
            if (mant) then
               ctoxd = ctoxd * 10.0_dp**(iexp-1)
               if (minus) ctoxd = -ctoxd
               return
            else
               i = -i
               return
            endif
         endif
         goto 20
      endif

      if (.not. mant) then
         i = -i
         return
      endif

      ! At this point we have a real with one of the following forms:  nnn.nnn   .nnn   nnn.  nnn
      ! and we have not yet reached the end of the string

      ! Check existence of an exponent:
      if ((string(i:i) .eq. 'd') .or. (string(i:i) .eq. 'D') .or. (string(i:i) .eq. 'e') .or. (string(i:i) .eq. 'E')) then
         i = i + 1
         if (i > len(string)) then
            ctoxd = ctoxd * 10.0_dp**(iexp-1)
            if (minus) ctoxd = -ctoxd
            return
         endif
       else
          ctoxd = ctoxd * 10.0_dp**(iexp-1)
          if (minus) ctoxd = -ctoxd
          return
      endif

      ! Read in exponent - first the sign (if any). Note that a trailing sign IS valid i.e will accept "1.0d+" or "-3.d-"
      if (string(i:i) .eq. '-') then
         i = i + 1
         if (i > len(string)) then
            ctoxd = ctoxd * 10.0_dp**(iexp-1)
            if (minus) ctoxd = -ctoxd
            return
         endif
         iminus = -1
      elseif (string(i:i) == '+') then
         i = i + 1
         if (i .gt. len(string)) then
            ctoxd = ctoxd * 10.0_dp**(iexp-1)
            if (minus) ctoxd = -ctoxd
            return
         endif
         iminus = +1
      else
         iminus = +1
      endif

      ! Now get the power of 10:
      ipwr = 0
30    continue
      d = index (digits, string(i:i))
      if (d .ne. 0) then
         ipwr = 10*ipwr + (d-1)
         i = i + 1
         if (i .gt. len(string)) then
            ctoxd = ctoxd * 10.0_dp**(iminus*ipwr+iexp-1)
            if (minus) ctoxd = -ctoxd
            return
         endif
         goto 30
      else
         ctoxd = ctoxd * 10.0_dp**(iminus*ipwr+iexp-1)
         if (minus) ctoxd = -ctoxd
         return
      endif

   end function ctoxd
   
   !----------------------------------------------------------------------------------------------------------------------
   !! Reads a real from a character string, starting at a given location. The real needs to be delimited
   !! from the rest of the string.
   !!
   !! The pointer i is placed after the delimiting character if an real is read; if an real is not read
   !! then abs(i)=position of offending character (compare geti).
   !!
   !!v      value = ctod (string, i)
   !!
   !!>   input:   string   Character string
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: If +ve: real was read; i is set to position of character immediately after the 
   !!                                  delimiting character (end of string, trailing space or comma).
   !!                           If -ve: no real read; abs(i) is the position of the character that caused the
   !!                                  problem (e.g. invalid delimiting character, or end of line reached).
   !!    output:    ctod   Value of real read from string. If there was a problem, ctoi = 0.
   !!<

   real(dp) function ctod (string, i)
      character(len=*), intent(in) :: string
      integer(i4b), intent(inout) :: i

      ctod = ctoxd(string,i)  
      if (i>0) then  ! successfully read undelimited real; now check if delimited
         i = idelim(string, i, NUMERIC_LIST_TERMINATOR)
         if (i<0) then
            ctod = 0
         endif
      endif

      return
   end function ctod
   
   !----------------------------------------------------------------------------------------------------------------------
   !! Reads a real from a character string, starting at a given location. The real needs to be delimited
   !! from the rest of the string. The pointer i is placed after the delimiting character (or at the start of
   !! delimiting trailing space).
   !!
   !! Equivalent to calling  getds(string, i, value, m [,iproblem]) with m=1
   !!
   !!v      n = getd (string, i, value [,iproblem])
   !!
   !!>   input:   string   Character string
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: Position immediately after the delimiting character (or at the start of delimiting
   !!                           trailing space) following succesfully read real.
   !!                           If the end of the line was reached without reading a real, then i = len(string)+1.
   !!              value   Value read from string (unchanged if problem)
   !!           iproblem   If real read (getd=1): set to zero
   !!                      If was not read (getd=0): set to position of problem character
   !!    output:    getd   Number of integers read from string (0 or 1).
   !!<
   ! Code should be identical to geti, apart from type changes (and geti=>getd)
   ! *** Consider making a call to getds with m=1 to guarantee equivalence

   integer(i4b) function getd (string, i, value, iproblem)
      character(len=*), intent(in):: string
      integer(i4b), intent(inout) :: i
      real(dp), intent(inout) :: value
      integer(i4b), intent(inout), optional :: iproblem
      integer(i4b) :: itemp
      real(dp) :: vtemp

      i = max(1,abs(i))
      itemp = i
      vtemp = ctod (string, itemp)
      if (itemp > 0) then  ! real read
         i = itemp
         value = vtemp
         if (present(iproblem)) iproblem = 0
         getd = 1
      else
         if (abs(itemp) > len(string)) then  ! reached end of line without reading integer
            i = len(string) + 1
            if (present(iproblem)) iproblem = 0
         else
            if (present(iproblem)) iproblem = -itemp
         endif
         getd = 0
      endif
      return

   end function getd

   !----------------------------------------------------------------------------------------------------------------------
   !! Reads a list of reals from a string, upto a specified maximum number of values. The reals must all be
   !! properly delimited from each other, and the final real delimited from the remainder of the string.
   !! The pointer i is placed after the delimiting character (or at the start of delimiting trailing space).
   !!
   !! If non-delimited reals were encountered, then getds gives the number of delimited reals read up until that
   !! point, i is placed after the delimitor past the last real read (or at the start of delimiting trailing space),
   !! and the (optional) argument iproblem the location of the offending character.
   !!
   !!v      nvals = getis (string, i, values, m [,iproblem])
   !!
   !!>   input:   string   Character string
   !!                  m   Maximum number of reals to be read
   !!    in/out:       i   i/p: Starting position in the string
   !!                           If i is -ve, it is interpreted internally as +ve
   !!                      o/p: Position immediately after the delimiting character (or at the start
   !!                          of delimiting trailing space), following the last real succesfully read.
   !!                           If the end of the line was reached without reading m reals, then i = len(string)+1.
   !!             values   Array whose first getds (below) values are filled from the string
   !!           iproblem   =0: No undelimited reals encountered (i.e. no syntactical problems)
   !!                      >0: Location of offending character if undelimited real encountered.
   !!    output:   getis   Number of reals read from the string.
   !!<
   integer(i4b) function getds (string, i, values, m, iproblem)
      character(len=*), intent(in) :: string
      integer(i4b), intent(inout) :: i
      real(dp), intent(inout) :: values(:)
      integer(i4b), intent(in) :: m
      integer(i4b), intent(inout), optional :: iproblem
      integer(i4b) :: m_max, isave
      real(dp) :: value

      i = max(1,abs(i))
      m_max = min(size(values),m)
      if (present(iproblem)) iproblem = 0
      getds = 0
      if (m < 1) return ! dummy function if no reals asked for
      do while (getds < m_max)
         isave = i   ! keep in case error in ctod
         value = ctod (string, i)
         if (i > 0) then   ! no problems
            getds = getds + 1
            values(getds) = value
         else
            if (abs(i) > len(string)) then   ! problem was simply because end of line was reached; this is OK
               i = len(string) + 1
            else
               if (present(iproblem)) iproblem = -i
               i = isave
            endif
            return
         endif
      end do
      
      return
   end function getds

   !----------------------------------------------------------------------------------------------------------------------
   !! Single precision version of ctoxd
   real(sp) function ctoxr (string, i)
      character(len=*), intent(in) :: string   
      integer(i4b), intent(inout) :: i
      ctoxr = real(ctoxd(string,i),sp)
   end function ctoxr

   !----------------------------------------------------------------------------------------------------------------------
   !! Single precision version of ctod
   real(sp) function ctor (string, i)
      character(len=*), intent(in) :: string   
      integer(i4b), intent(inout) :: i
      ctor = real(ctod(string,i),sp)
   end function ctor

   !----------------------------------------------------------------------------------------------------------------------
   !! Single precision version of getd
   integer(i4b) function getr (string, i, value, iproblem)
      character(len=*), intent(in):: string
      integer(i4b), intent(inout) :: i
      real(sp), intent(inout) :: value
      integer(i4b), intent(inout), optional :: iproblem
      real(dp) value_temp
      if (present(iproblem)) then
         getr = getd(string, i, value_temp, iproblem)
      else
         getr = getd(string, i, value_temp)
      endif
      value = value_temp
      return
   end function getr
      
   !----------------------------------------------------------------------------------------------------------------------
   !! Single precision version of getds
   integer(i4b) function getrs (string, i, values, m, iproblem)
      character(len=*), intent(in) :: string
      integer(i4b), intent(inout) :: i
      real(sp), intent(inout) :: values(:)
      integer(i4b), intent(in) :: m
      integer(i4b), intent(inout), optional :: iproblem
      real(dp) :: values_temp(min(size(values),max(0,m)))
      if (present(iproblem)) then
         getrs = getds(string, i, values_temp, m, iproblem)
      else
         getrs = getds(string, i, values_temp, m)
      endif
      if (getrs > 0) then
         values(1:getrs) = real(values_temp(1:getrs),sp)
      endif
      return
   end function getrs
   !----------------------------------------------------------------------------------------------------------------------
!C      integer function ITOC (n, string)
!C
!C  ITOC: converts integer N to character STRING, followed by
!C       trailing blanks. If STRING is too short to hold all
!C       of N, then only the least significant part of N is held,
!C       (including loss of the sign of N if N is negative).
!C       ITOC contains the number of characters actually used to hold N.
!C
!C Arguments
!C ---------
!C    integer      N  integer variable or constant
!C              i/p:  integer to be written to STRING
!C              o/p:  [unchanged]
!C
!C    ch*(*)  STRING  character variable
!C              i/p:  irrelevant
!C              o/p:  contains the character formatted version of N
!C                   padded with trailing blanks. If STRING too
!C                   short to hold N, then only the least significant
!C                   digits are held, and the -ve sign lost if N is
!C                   negative.
!C
!C Function
!C --------
!C  integer     ITOC
!C              i/p:  -
!C              o/p:  number of characters filled by ITOC
!C
!C
!C Subroutine calls
!C ----------------
!C
!C Comments
!C --------
!C
!C-----------------------------------------------------------------------
      integer function ITOC (n, string)
      integer(i4b):: n, i, j, nabs, d
      character(len=*):: string
      character(len=1):: digits(10), temp
      save digits
      data digits/'0','1','2','3','4','5','6','7','8','9'/

! fill STRING with N in reverse order of characters:
      nabs = ABS(n)
      do 10 i = 1, LEN(string)
         d = MOD(nabs, 10)
         string(i:i) = digits(d+1)
         ITOC = i
         nabs = nabs / 10
         if (nabs .eq. 0) go to 100
 10   continue
 100  if ((n .lt. 0) .and. (ITOC .lt. LEN(string))) then
         ITOC = ITOC + 1
         string(ITOC:ITOC) = '-'
      end if

! now invert the order of the characters:
      if (ITOC .gt. 1) then
         do 20 i = 1, ITOC/2
            j = ITOC - i + 1
            temp = string(i:i)
            string(i:i) = string(j:j)
            string(j:j) = temp
 20      continue
      endif

! pad the remainder of the string with blanks:
      if (ITOC .lt. LEN(string)) then
         do 30 i = ITOC+1, LEN(string)
            string(i:i) = ' '
 30      continue
      endif

      return
      end function ITOC
!C     integer function GETSTR (string, i, substr)
!C
!C Starts at the position I and reads in a sub-string from the string,
!C skipping leading blanks and tabs. The string is read until:
!C   (1) the end of the sub-string is reached (see below)
!C   (2) the end of the line is reached
!C   (3) SUBSTR is filled up.
!C The pointer I is placed immediately after the end of the sub-string
!C GETSTR is returned as the number of characters read into SUBSTR. In
!C case (3) the function "reads ahead" to check if SUBSTR continues;
!C if it does then GETSTR is returned as negative. The pointer I is
!C placed after the sub-string regardless of whether or not SUBSTR
!C was long enough to hold it.
!C
!C If no substring is read, then SUBSTR is set to blanks
!C
!C
!C A "sub-string" is a sequence of characters terminated by a space
!C  eg.  %ksir**,>//?
!C
!C Spaces can be included by enclosing the string with speech marks,
!C ie. by "
!C  eg.  "hello there!"
!C
!C If the string is not started with speech marks, they are read as
!C part of the string
!C  eg.  Fred:"GO!"
!C is interpreted as typed. If the string is started with speech
!C marks then any speech marks must be typed as "" within the string
!C  eg.   "he said """"Clear off!"" was the reply."""
!C becomes
!C         he said ""Clear off!" was the reply."
!C The string is terminated when there are an odd number of succesive "
!C with the last " as the enclosing speech mark.
!C
!C Note that with strings enclosed by speech marks, a delimiting space
!C after the terminating " is not necessary.
!C  eg.   "any old"iron
!C will be read as:
!C         any old
!C A terminating " is not required. If it is omitted, then the remainder
!C of the line is read into SUBSTR, with trailing blanks and tabs
!C removed. The pointer will be placed at the end of the line.
!C  eg. STRING = '   "lookit that !      '
!C                                      I=^
!C   &  SUBSTR =     'lookit that !'
!C
!C
!C Arguments
!C ---------
!C   ch*(*)   STRING  character variable or constant
!C              i/p:  input string
!C              o/p:  [unchanged]
!C
!C   integer       I  integer variable
!C              i/p:  starting position in STRING
!C              i/p:  starting position in STRING
!C                               I  < -LEN(STRING)    return; I -> I
!C                LEN(STRING) =< I  <  0              I -> -I
!C                               I  =  0              I ->  1
!C                          1 =< I =<  LEN(STRING)    OK
!C                LEN(STRING)  < I                    return; I unchanged
!C
!C              o/p:  placed immediately after the end of the sub-string
!C
!C   ch*(*)   SUBSTR  character variable
!C              i/p:  [irrelevant]
!C              o/p:  the sub-string read from the string, padded if
!C                    necessary by trailing blanks
!C Function
!C --------
!C   integer GETSTR
!C              i/p:  -
!C              o/p:  If +ve (or 0):
!C                     Number of characters in the sub-string read
!C                     from line
!C
!C Subroutine calls
!C ----------------
!C
!C Comments
!C --------
!C   Trailing blanks in the sub string ARE significant, as the value
!C of GETSTR includes blanks in the substring. For example
!C    hello      -> GETSTR=5
!C   "hello  "   -> GETSTR=7
!C
!C
!C---------------------------------------------------------------------
      integer function GETSTR (string, i, substr)
      integer(i4b):: i, k, imark, ltrim
      character(len=*):: string, substr
      GETSTR = 0
      do 10 k = 1, LEN(substr)
         substr(k:k) = ' '
 10   continue
!
! Deal with case of I outside 1 to LEN(STRING)
! --------------------------------------------
      if (i .lt. -LEN(string)) then
         i = -i
         return
      else if (i .lt. 0) then
         i = -i
      else if (i .eq. 0) then
         i = 1
      else if (i .gt. LEN(string)) then
         return
      endif
! Read SUBSTRING
! --------------
      i = INEXT (string,i)
      if (i .gt. LEN(string)) return
!
      if (string(i:i) .eq. '"') then
         imark = 0
         ltrim = 0
 100     i = i + 1
         if (i .gt. LEN(string)) then
            if (imark .eq. 0) then
               if (ltrim .lt. min(GETSTR,LEN(substr))) then
                  do 110 k = ltrim+1, min(GETSTR,LEN(substr))
                     substr(k:k) = ' '
 110              continue
               endif
               GETSTR = ltrim
            endif
            if (GETSTR .gt. LEN(substr)) GETSTR = LEN(substr)
            return
         endif
         if (string(i:i) .eq. '"') then
            if (imark .eq. 0) then
               imark = 1
            else
               imark = 0
               GETSTR= GETSTR + 1
               ltrim = GETSTR
               if (GETSTR .le. LEN(substr)) substr(GETSTR:GETSTR) = '"'
            endif
         else
            if (imark .eq. 0) then
               GETSTR= GETSTR + 1
               if (INEXT(string,i) .eq. i) ltrim = GETSTR
               if (GETSTR .le. LEN(substr)) substr(GETSTR:GETSTR) =string(i:i)
            else
               if (GETSTR .gt. LEN(substr)) GETSTR = LEN(substr)
               return
            endif
         endif
         goto 100
      else
         GETSTR = 1
         substr(1:1) = string(i:i)
 200     i = i + 1
         if (i .gt. LEN(string)) then
            if (GETSTR .gt. LEN(substr)) GETSTR = LEN(substr)
            return
         endif
         if (INEXT(string,i) .eq. i) then
            GETSTR= GETSTR + 1
            if (GETSTR .le. LEN(substr)) substr(GETSTR:GETSTR) =  string(i:i)
            goto 200
         else
            if (GETSTR .gt. LEN(substr)) GETSTR = LEN(substr)
            return
         endif
      endif

      end function GETSTR
      
!C     integer function ICMD (clist, m, cmd)
!C
!C   Searches for the command CMD in the list of M commands CLIST, and
!C  returns ICMD as position of CMD in CLIST.
!C   The subroutine strips away leading and trailing blanks in CMD and
!C  CLIST before comparison. Equality is then defined when the first
!C  LEN(CMD) characters of an element of CLIST equal CMD, ignoring case
!C  differences. Clearly, this cannot happen if the element of CLIST
!C  is shorter than CMD.
!C
!C  -M =< ICMD =<-1    more than one element of CLIST equals CMD;|ICMD|
!C                    set to the first occurence
!C        ICMD =  0    CMD does not appear
!C   1 =< ICMD =< M    single equality
!C
!C   If CMD is blank then ICMD is returned as 0
!C   If M =<0 then ICMD is returned as 0
!C
!C  Note: CMD can be any character string, including blanks or tabs if
!C contained between other characters. Leading and trailing blanks and
!C tabs are stripped, however.
!C
!C
!C
!C Arguments
!C ---------
!C   ch*(*) CLIST(M)  character array
!C              i/p:  list of commands
!C              o/p:  [unchanged]
!C
!C   integer      M   integer variable or constant
!C              i/p:  number of commands in the list
!C              o/p:  [unchanged]
!C
!C   ch*(*)     CMD   character variable or constant
!C              i/p:  command
!C              o/p:  [unchanged]
!C
!C Function
!C --------
!C   integer   ICMD
!C              i/p:  -
!C              o/p:  index of CMD in CLIST
!C         -M =< ICMD =<-1    more than one element of CLIST equals CMD
!C                           |ICMD| set to the first occurence
!C               ICMD =  0    CMD does not appear, or CMD is blank
!C          1 =< ICMD =< M    single equality
!C
!C
!C Subroutine calls
!C ----------------
!C    UPCASE, LENSTR, INEXT, ERROR
!C
!C Comments
!C --------
!C---------------------------------------------------------------------
      integer function ICMD (clist, m, cmd)
      integer m, j, k, begcmd, endcmd, lencmd, begc, endc, lenc
      character*1 chcmd, chc
      character(len=*):: clist(*), cmd
      logical match, preveq

      if (m .le. 0) then
         ICMD = 0
         return
      endif

      endcmd = LENSTR(cmd)
      if (endcmd .ne. 0) then
         begcmd = INEXT(cmd,1)
         lencmd = endcmd - begcmd + 1
         ICMD = 0
         do 10 j = 1, m
            endc = LENSTR(clist(j))
            if (endc .ne. 0) then
               begc = INEXT(clist(j),1)
               lenc = endc - begc + 1
! Check equality if length of CMD is less than or equal to that of CLIST(J):
               if (lencmd .le. lenc) then
!        Check if a valid abbreviation:
                  do 20 k = 1, lencmd
                     chcmd = cmd(begcmd+k-1:begcmd+k-1)
                     chc   = clist(j)(begc+k-1:begc+k-1)
                     call UPCASE (chcmd)
                     call UPCASE (chc)
                     if (chcmd .ne. chc) goto 11
 20               continue
!        Indicate true equality:
                  if (lencmd .eq. lenc) then
                     match = .TRUE.
                  else
                     match = .FALSE.
                  endif
!        Check if a previous valid abbreviation found
                  if (ICMD .eq. 0) then
                     ICMD = j
                     preveq = match
                  else
                     if (preveq .and. match) then
                        ICMD = -ICMD
                        return
                     else if (preveq .and. (.not. match)) then
                        continue
                     else if ((.not. preveq) .and. match) then
                        ICMD = j
                        preveq = .TRUE.
                     else if ((.not. preveq) .and. (.not. match)) then
                        ICMD = -abs(ICMD)
                     endif
                  endif
               endif
            endif
 11         continue
 10      continue
         return
      else
         ICMD = 0
         return
      endif

      end function ICMD
      
       integer function NEWFL (iunit, flname, access)
!C
!C  Utility function to open a new file
!C  ACCESS is a constant, READ or READWR to determine if a 
!C read only file or access is READ and WRITE.
!C  Function value is returned as OK all fine, or ERR otherwise
!C
      integer iunit, access
      character(len=*) flname

      newfl = CONNFL (iunit, flname, access, NEW)
      return
      end function NEWFL

      integer function OLDFL (iunit, flname, access)
!C
!C  Utility function to open a pre-existing file
!C  ACCESS is a constant, READ or READWR to determine if a 
!C read only file or access is READ and WRITE.
!C  Function value is returned as OK all fine, or ERR otherwise
!C
      integer iunit, access
      character*(*) flname

!      include 'tools.par'

      oldfl = CONNFL (iunit, flname, access, OLD)
      return
      end function OLDFL
      
      integer function CONNFL (iunit, flname, access, status)
      integer iunit, status, access
      character(len=*):: flname
      !include 'tools.par'
!C-----------------------------------------------------------------------
!C  Utility for connecting a file to a given unit
!C
!C-----------------------------------------------------------------------
      integer istatus
      logical uexist, uopen, fexist, fopen

      connfl = ERR
!C
!C Check that unit number exists and is not currently used:
      inquire (unit=iunit, exist=uexist, opened=uopen)
      if (.not. uexist) then
         call remark (' Unit number does not exist (CONNFL)')
         return
      endif
      if (uopen) then
         call remark (' Unit number already opened (CONNFL)')
         return
      endif
!C
!C Check that the file name is not blank:
      if (lenstr(flname) .eq. 0) then
         call remark (' File name has zero length (CONNFL)')
         return
      endif
!C
!C Check if file exists and/or is open:
      inquire (file=flname, exist=fexist, opened=fopen)

      if (status .eq. OLD) then
!C-----------------------------------------------------------------------
!C Old file:
         if (fopen) then
            call remark (' File is already open (CONNFL)')
            return
         endif
         if (.not. fexist) then
            call remark (' File does not exist (CONNFL)')
            return
         endif
         if (access .eq. READ) then
            istatus = sys_open (iunit, flname, 'OLD', 'READONLY')
         else
            istatus = sys_open (iunit, flname, 'OLD', ' ')
         endif
         if (istatus .ne. 0) goto 1000
         connfl = OK
         return
      else if (status .eq. NEW) then
!C-----------------------------------------------------------------------
!C New file:
         if (access .eq. READ) then
            call remark(' Read only access not permitted for a new file (CONNFL)')
            return
         else
            istatus = sys_open (iunit, flname, 'NEW', ' ')
            if (istatus .ne. 0) goto 1000
            connfl = OK
            return
         endif
      else
!C-----------------------------------------------------------------------
!C OLD/NEW:

!C   try to open an old file first:
         if (fopen) then
            call remark (' File is already open (CONNFL)')
            return
         endif
         if (.not. fexist) goto 10
         if (access .eq. READ) then
            istatus = sys_open (iunit, flname, 'OLD', 'READONLY')
         else
            istatus = sys_open (iunit, flname, 'OLD', ' ')
         endif
         if (istatus .ne. 0) goto 1000
         connfl = OK
         return

!C   now try a new file:
 10      continue
         if (access .eq. READ) then
            call remark(' Read only access not permitted for a new file (CONNFL)')
            return
         else
            istatus = sys_open (iunit, flname, 'NEW', ' ')
            if (istatus .ne. 0) goto 1000
            connfl = OK
            return
         endif
      endif
!C-----------------------------------------------------------------------
 1000 call remark (' Error opening file (CONNFL)')
      return

      end function CONNFL
      
    integer function SYS_OPEN (iunit, flname, status, readonly)
	integer iunit
	character(len=*):: flname, status, readonly
!C
!C Function that opens a file
!C Operating system dependent: VMS put     status_new = 'NEW'
!C                             UNIX or NT, status_new = 'REPLACE'
!C
!C Comments
!C --------
!C  *** system/compiler specific
!C        Uses Digital Fortran extensions to OPEN:
!C              READONLY, SHARED, CARRIAGE_CONTROL='LIST'
!C
	character*20 internal_status

	internal_status = status
!C
!C The following line can be removed if version numbering is supported:
	if (internal_status .eq. 'NEW') internal_status = 'REPLACE'


	if (readonly .eq. 'READONLY') then
		if (status .eq. 'OLD') then
			open (unit=iunit, file=flname, status=internal_status,err=1000, action='read')
		else
			goto 1000		! Cannot have READONLY for a new file
		endif
	else
		if (status .eq. 'OLD') then
			open (unit=iunit, file=flname, status=internal_status,err=1000)
		else
			open (unit=iunit, file=flname, status=internal_status,form='FORMATTED', err=1000, recl=2048)
		endif
	endif
	sys_open = 0
	return
!C
!C Error condition:
 1000	continue
	sys_open = 1
	return

	end function SYS_OPEN


	subroutine read_line_write (iunit, line, len_line, iunit_write, last_line, ierr_write)
!
! Reads a line from the file, skipping over blank lines and those beginning with an exclamation mark
! If error reading, then LINE set to blank, len_line = ERR
! If end-of-file, then LINE set to blanks, len_line = EOF
!
! If iunit_write .ne. 0 will also write all the read lines to a file; writing is not performed if iunit_write=0
! if last_line=.true. then writes the returned line, if last_line=.false. then doesn't
! IERR_WRITE=0 if writing is OK, otherwise /=0

	integer iunit, iunit_write, len_line, i,  ibeg,  ierr_write
	logical last_line
	character(len=*) line
!
	ierr_write=OK
 1000	len_line = getlf (iunit, line)
	if (len_line .gt. 0) then
		if (iunit_write .ne. 0 .and. last_line) then
			ierr_write = putlf (iunit_write, line)
			if (ierr_write .ne. OK) then	! error writing to output file, so return error
				len_line=ERR
				call cspace(line)
				return
			endif
		endif
		i = 1
		ibeg = inext(line,i)
		if (line(ibeg:ibeg) .eq. '!') then
			goto 1000
		else
			return
		endif
	else if (len_line .eq. 0) then
		if (iunit_write .ne. 0) then
			ierr_write = putlf (iunit_write, line)
			if (ierr_write .ne. OK) then	! error writing to output file, so return error
				len_line=ERR
				call cspace(line)
				return
			endif
		endif
		goto 1000
	else if (len_line .eq. ERR) then 
		call cspace (line)
		return
	else if (len_line .eq. EOF) then
		call cspace (line)
		return
	else
		call remark (' *** UNKNOWN ERROR - CONTACT T.G.PERRING')
		stop
	endif

	end subroutine read_line_write

!C     integer function PUTLF (iunit, line)
!C
!C  Outputs line from onto a given unit number stream, stripping
!C trailing blanks and tabs
!C
!C Arguments
!C ---------
!C   integer   IUNIT  integer constant or variable
!C              i/p:  unit number for input
!C              o/p:  [unchanged]
!C
!C   ch*(*)     LINE  character variable or constant
!C              i/p:  line to be written to IUNIT
!C              o/p:  [unchanged]
!C
!C Function
!C --------
!C   integer   PUTLF
!C              i/p:  -
!C              o/p:  If  OK: no problems
!C                    If ERR: problem writing to IUNIT
!C                           = EOF  if end of file
!C                           = ERR  if error
!C
!C Subroutine calls
!C ----------------
!C   LENSTR
!C
!C Comments:
!C ---------
!C   Suitable for files opened with the DEC Fortran keyword
!C  carriagecontrol='LIST'
!C
!C-----------------------------------------------------------------------
      integer function PUTLF (iunit, line)
      integer iunit
      character(len=*) line

      integer lline, i

      lline = lenstr (line)
      if (iunit .eq. STDOUT) then
         if (lline .eq. 0) then
            write (*, '('' '')', err=100)
         else
            write (*, '('' '',a)', err=100) line(1:lline)
         endif
      else
         if (lline .eq. 0) then
            write (iunit, '(a)', err=100)
         else
            write (iunit, '(a)', err=100) line(1:lline)
         endif
      endif
      putlf = OK
      return

 100  putlf = ERR
      return

      end function putlf
!C      integer function CTOC (instr, outstr)
!C
!C  Copies from one character string to another, once the input string
!C is stripped of leading blanks. OUTSTR is padded with trailing blanks
!C if OUTSTR is longer than INSTR; if OUTSTR is shorter, the leading
!C characters of INSTR are coped to OUTSTR
!C
!C Arguments
!C ---------
!C
!C    ch*(*)   INSTR  character variable
!C              i/p:  input string
!C              o/p:  [unchanged]
!C
!C    ch*(*)  OUTSTR  character variable
!C              i/p:  irrelevant
!C              o/p:  output string
!C
!C Function
!C --------
!C  integer     CTOC
!C              i/p:  -
!C              o/p:  number of characters filled by CTOC
!C
!C-----------------------------------------------------------------------
      subroutine CTOC (instr, outstr)
      integer i, ibeg, iend, lenin, lenout
      character(len=*):: instr, outstr
 

      ibeg = inext(instr,1)
      iend = lenstr(instr)
      lenin  = max(0,iend-ibeg+1)
      lenout = len(outstr)
      if (lenin .ge. lenout) then
         outstr = instr(ibeg:ibeg+lenout-1)
 !        ctoc   = lenout
      else
         if (lenin .gt. 0) outstr(1:lenin) = instr(ibeg:iend)
         do 10 i = lenin+1, lenout
            outstr(i:i) = ' '
 10      continue
!         ctoc   = lenin
      endif

      return
      end subroutine CTOC
!C     integer function GETWRD (string, i, word)
!C
!C Starts at the position I and reads in a word from the string,
!C skipping leading blanks and tabs. The string is read until:
!C   (1) an invalid character is read from the string
!C   (2) the end of the line is reached
!C   (3) WORD is filled up.
!C The pointer I is placed immediately after the end of the word
!C GETWRD is returned as the number of characters read into WORD. In
!C case (3) the function "reads ahead" to check if the word continues;
!C if it does then GETWRD is returned as negative. The pointer I is
!C placed after the word regardless of whether or not WORD was long
!C enough to hold it.
!C
!C If no word is read, then WORD is set to blanks
!C
!C A "word" is a sequence of characters consisting only of:
!C     a-z  A-Z  0-9  $  _
!C It differs form a "name" only in that the first character can be
!C any of the above characters, not just alphabetical.
!C
!C
!C Arguments
!C ---------
!C   ch*(*)   STRING  character variable or constant
!C              i/p:  input string
!C              o/p:  [unchanged]
!C
!C   integer       I  integer variable
!C              i/p:  starting position in STRING
!C              i/p:  starting position in STRING
!C                               I  < -LEN(STRING)    return; I -> -I
!C                LEN(STRING) =< I  <  0              I -> -I
!C                               I  =  0              I ->  1
!C                          1 =< I =<  LEN(STRING)    OK
!C                LEN(STRING)  < I                    return; I unchanged
!C
!C              o/p:  placed immediately after the end of the word, or
!C                    the first invalid character
!C                     (1) the first invalid character, even if leading
!C                        blanks were skipped.
!C                          e.g. "  hello! I'm home"
!C                                       ^
!C                          e.g. "    /all"
!C                                    ^
!C                          e.g. "        "
!C                                        ^
!C                     (2) the end of the line is reached,
!C                          e.g. "  wotcha"
!C                                        ^
!C                     (3) WORD was too short
!C                          e.g. 6 character word & "   blessed ants"
!C                               WORD = "blesse",   i at       ^
!C
!C   ch*(*)     WORD  character variable
!C              i/p:  [irrelevant]
!C              o/p:  the word read from the string, padded if
!C                    necessary by trailing blanks
!C Function
!C --------
!C   integer GETWRD
!C              i/p:  -
!C              o/p:  If +ve (or 0):
!C                     Number of characters in the word read from line
!C
!C Subroutine calls
!C ----------------
!C  INEXT, LALFCH, LNAMCHC
!C
!C Comments
!C --------
!C---------------------------------------------------------------------
      integer function GETWRD (string, i, word)
      integer i, k, ibeg
      character(len=*):: string, word

      GETWRD = 0
      do 10 k = 1, LEN(word)
         word(k:k) = ' '
 10   continue
!C
!C Deal with case of I outside 1 to LEN(STRING)
!C --------------------------------------------
      if (i .lt. -LEN(string)) then
         i = -i
         return
      else if (i .lt. 0) then
         i = -i
      else if (i .eq. 0) then
         i = 1
      else if (i .gt. LEN(string)) then
         return
      endif
!C
!C Read WORD
!C ---------
      i = INEXT (string,i)
      if (i .gt. LEN(string)) return
      if (.not. LNAMCH(string(i:i))) return

      ibeg = i
 20   i = i + 1
      if ( i .le. LEN(string) ) then
         if (LNAMCH(string(i:i))) goto 20
      endif
      if (i-ibeg .le. LEN(word)) then
         GETWRD = i-ibeg
      else
         GETWRD = LEN(word)
      endif
      word(1:abs(GETWRD)) = string(ibeg:ibeg+abs(GETWRD)-1)

      return
      end function GETWRD
!C      logical function LALFCH(string)
!C  Checks if the first character of a string is an alphabetical
!C character i.e. is one of a-z, A_Z.
!C Returns as .FALSE. if not; .TRUE. if it is.
!C
      logical function LALFCH(string)
      character*(*) string
      if ( (LGE(string(1:1),'A') .and. LLE(string(1:1),'Z')) .or. &
          (LGE(string(1:1),'a') .and. LLE(string(1:1),'z')) ) then
         LALFCH = .TRUE.
      else
         LALFCH = .FALSE.
      endif

      return

      end function LALFCH
!C      logical function LNAMCH(string)
!C  Checks if the first character of a string is a valid character
!C for a symbolic name i.e. is one of a-z, A_Z, 0-9, $, _, %
!C Returns as .FALSE. if not; .TRUE. if it is.
!C
      logical function LNAMCH(string)
      character*(*) string
      if ( (LGE(string(1:1),'A') .and. LLE(string(1:1),'Z')) .or. &
          (LGE(string(1:1),'a') .and. LLE(string(1:1),'z')) .or. &
          (LGE(string(1:1),'0') .and. LLE(string(1:1),'9')) .or. & 
          (string(1:1) .eq. '$') .or. (string(1:1) .eq. '_') .or. &
          (string(1:1) .eq. '%') ) then
         LNAMCH = .TRUE.
      else
         LNAMCH = .FALSE.
      endif

      return

      end function LNAMCH

      
	end module IXMtools
