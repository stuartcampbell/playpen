!----------------------------------------------------------------------------------------------------------------------
! MODULE: IXMsort
!----------------------------------------------------------------------------------------------------------------------
!! A set of sorting and related routines.
!!
!! Uses Michael Olagnon's collection in Orderpack as the core algorithms.
!! See http://www.fortran-2000.com/rank/index.html#4.1
!!       Ranking: Use mrgrnk, unirnk
!! Sort in place: Use refsor
!!        Median: Use valmed
!!
!! Olagnon collects the integer, real and double routines for one operation in a module, so there is a
!! module for ranking, one for in-place sorting etc. The modules that we need have had the minimum edit
!! i.e. change the name of the sole public routine in each module to conform to our naming scheme. THis
!! practive should be followed if further modules are taken from Orderpack.
!! The modules are collected as just the one, IXMsort, at the bottom of the code - this enables compilation
!! on one sweep.
!----------------------------------------------------------------------------------------------------------------------
!  Ranking:   call IXFrank (array_to_order, rank)
!             call IXFunique_rank (array_to_order, rank, n_unique)
!
!  Sorting:   call IXFsort (array_to_order)
!
!  Median:    median_value = IXFmedian(array)
!----------------------------------------------------------------------------------------------------------------------

!==============================================================================
Module m_refsor
  Integer, Parameter :: kdp = selected_real_kind(15)
  public :: IXFsort   ! renmae from refsor
  private :: kdp
  private :: R_refsor, I_refsor, D_refsor
  private :: R_inssor, I_inssor, D_inssor
  private :: R_subsor, I_subsor, D_subsor
  interface IXFsort
    module procedure d_refsor, r_refsor, i_refsor
  end interface IXFsort
contains

Subroutine D_refsor (XDONT)
!  Sorts XDONT into ascending order - Quicksort
! __________________________________________________________
!  Quicksort chooses a "pivot" in the set, and explores the
!  array from both ends, looking for a value > pivot with the
!  increasing index, for a value <= pivot with the decreasing
!  index, and swapping them when it has found one of each.
!  The array is then subdivided in 2 ([3]) subsets:
!  { values <= pivot} {pivot} {values > pivot}
!  One then call recursively the program to sort each subset.
!  When the size of the subarray is small enough, one uses an
!  insertion sort that is faster for very small sets.
!  Michel Olagnon - Apr. 2000
! __________________________________________________________
! __________________________________________________________
      Real (kind=kdp), Dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
!
!
      Call D_subsor (XDONT, 1, Size (XDONT))
      Call D_inssor (XDONT)
      Return
End Subroutine D_refsor
Recursive Subroutine D_subsor (XDONT, IDEB1, IFIN1)
!  Sorts XDONT from IDEB1 to IFIN1
! __________________________________________________________
      Real(kind=kdp), dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
      Real(kind=kdp) :: XPIV, XWRK
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (XDONT(IMIL) < XDONT(IDEB)) Then
            XWRK = XDONT (IDEB)
            XDONT (IDEB) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
         End If
         If (XDONT(IMIL) > XDONT(IFIN)) Then
            XWRK = XDONT (IFIN)
            XDONT (IFIN) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
            If (XDONT(IMIL) < XDONT(IDEB)) Then
               XWRK = XDONT (IDEB)
               XDONT (IDEB) = XDONT (IMIL)
               XDONT (IMIL) = XWRK
            End If
         End If
         XPIV = XDONT (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if XDONT (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (XDONT(ICRS) > XPIV) Exit
            End Do
            Do
               If (XDONT(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = XDONT (IDCR)
            XDONT (IDCR) = XDONT (ICRS)
            XDONT (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call D_subsor (XDONT, IDEB1, ICRS-1)
         Call D_subsor (XDONT, IDCR, IFIN1)
      End If
      Return
   End Subroutine D_subsor
   Subroutine D_inssor (XDONT)
!  Sorts XDONT into increasing order (Insertion sort)
! __________________________________________________________
      Real(kind=kdp), dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
      Integer :: ICRS, IDCR
      Real(kind=kdp) :: XWRK
!
      Do ICRS = 2, Size (XDONT)
         XWRK = XDONT (ICRS)
         If (XWRK >= XDONT(ICRS-1)) Cycle
         XDONT (ICRS) = XDONT (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
      End Do
!
      Return
!
End Subroutine D_inssor
!
Subroutine R_refsor (XDONT)
!  Sorts XDONT into ascending order - Quicksort
! __________________________________________________________
!  Quicksort chooses a "pivot" in the set, and explores the
!  array from both ends, looking for a value > pivot with the
!  increasing index, for a value <= pivot with the decreasing
!  index, and swapping them when it has found one of each.
!  The array is then subdivided in 2 ([3]) subsets:
!  { values <= pivot} {pivot} {values > pivot}
!  One then call recursively the program to sort each subset.
!  When the size of the subarray is small enough, one uses an
!  insertion sort that is faster for very small sets.
!  Michel Olagnon - Apr. 2000
! __________________________________________________________
! _________________________________________________________
      Real, Dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
!
!
      Call R_subsor (XDONT, 1, Size (XDONT))
      Call R_inssor (XDONT)
      Return
End Subroutine R_refsor
Recursive Subroutine R_subsor (XDONT, IDEB1, IFIN1)
!  Sorts XDONT from IDEB1 to IFIN1
! __________________________________________________________
      Real, dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
      Real :: XPIV, XWRK
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (XDONT(IMIL) < XDONT(IDEB)) Then
            XWRK = XDONT (IDEB)
            XDONT (IDEB) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
         End If
         If (XDONT(IMIL) > XDONT(IFIN)) Then
            XWRK = XDONT (IFIN)
            XDONT (IFIN) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
            If (XDONT(IMIL) < XDONT(IDEB)) Then
               XWRK = XDONT (IDEB)
               XDONT (IDEB) = XDONT (IMIL)
               XDONT (IMIL) = XWRK
            End If
         End If
         XPIV = XDONT (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if XDONT (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (XDONT(ICRS) > XPIV) Exit
            End Do
            Do
               If (XDONT(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = XDONT (IDCR)
            XDONT (IDCR) = XDONT (ICRS)
            XDONT (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call R_subsor (XDONT, IDEB1, ICRS-1)
         Call R_subsor (XDONT, IDCR, IFIN1)
      End If
      Return
   End Subroutine R_subsor
   Subroutine R_inssor (XDONT)
!  Sorts XDONT into increasing order (Insertion sort)
! __________________________________________________________
      Real, dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
      Integer :: ICRS, IDCR
      Real :: XWRK
!
      Do ICRS = 2, Size (XDONT)
         XWRK = XDONT (ICRS)
         If (XWRK >= XDONT(ICRS-1)) Cycle
         XDONT (ICRS) = XDONT (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
      End Do
!
      Return
!
End Subroutine R_inssor
!
Subroutine I_refsor (XDONT)
!  Sorts XDONT into ascending order - Quicksort
! __________________________________________________________
!  Quicksort chooses a "pivot" in the set, and explores the
!  array from both ends, looking for a value > pivot with the
!  increasing index, for a value <= pivot with the decreasing
!  index, and swapping them when it has found one of each.
!  The array is then subdivided in 2 ([3]) subsets:
!  { values <= pivot} {pivot} {values > pivot}
!  One then call recursively the program to sort each subset.
!  When the size of the subarray is small enough, one uses an
!  insertion sort that is faster for very small sets.
!  Michel Olagnon - Apr. 2000
! __________________________________________________________
! __________________________________________________________
      Integer, Dimension (:), Intent (InOut)  :: XDONT
! __________________________________________________________
!
!
      Call I_subsor (XDONT, 1, Size (XDONT))
      Call I_inssor (XDONT)
      Return
End Subroutine I_refsor
Recursive Subroutine I_subsor (XDONT, IDEB1, IFIN1)
!  Sorts XDONT from IDEB1 to IFIN1
! __________________________________________________________
      Integer, dimension (:), Intent (InOut) :: XDONT
      Integer, Intent (In) :: IDEB1, IFIN1
! __________________________________________________________
      Integer, Parameter :: NINS = 16 ! Max for insertion sort
      Integer :: ICRS, IDEB, IDCR, IFIN, IMIL
      Integer :: XPIV, XWRK
!
      IDEB = IDEB1
      IFIN = IFIN1
!
!  If we don't have enough values to make it worth while, we leave
!  them unsorted, and the final insertion sort will take care of them
!
      If ((IFIN - IDEB) > NINS) Then
         IMIL = (IDEB+IFIN) / 2
!
!  One chooses a pivot, median of 1st, last, and middle values
!
         If (XDONT(IMIL) < XDONT(IDEB)) Then
            XWRK = XDONT (IDEB)
            XDONT (IDEB) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
         End If
         If (XDONT(IMIL) > XDONT(IFIN)) Then
            XWRK = XDONT (IFIN)
            XDONT (IFIN) = XDONT (IMIL)
            XDONT (IMIL) = XWRK
            If (XDONT(IMIL) < XDONT(IDEB)) Then
               XWRK = XDONT (IDEB)
               XDONT (IDEB) = XDONT (IMIL)
               XDONT (IMIL) = XWRK
            End If
         End If
         XPIV = XDONT (IMIL)
!
!  One exchanges values to put those > pivot in the end and
!  those <= pivot at the beginning
!
         ICRS = IDEB
         IDCR = IFIN
         ECH2: Do
            Do
               ICRS = ICRS + 1
               If (ICRS >= IDCR) Then
!
!  the first  >  pivot is IDCR
!  the last   <= pivot is ICRS-1
!  Note: If one arrives here on the first iteration, then
!        the pivot is the maximum of the set, the last value is equal
!        to it, and one can reduce by one the size of the set to process,
!        as if XDONT (IFIN) > XPIV
!
                  Exit ECH2
!
               End If
               If (XDONT(ICRS) > XPIV) Exit
            End Do
            Do
               If (XDONT(IDCR) <= XPIV) Exit
               IDCR = IDCR - 1
               If (ICRS >= IDCR) Then
!
!  The last value < pivot is always ICRS-1
!
                  Exit ECH2
               End If
            End Do
!
            XWRK = XDONT (IDCR)
            XDONT (IDCR) = XDONT (ICRS)
            XDONT (ICRS) = XWRK
         End Do ECH2
!
!  One now sorts each of the two sub-intervals
!
         Call I_subsor (XDONT, IDEB1, ICRS-1)
         Call I_subsor (XDONT, IDCR, IFIN1)
      End If
      Return
   End Subroutine I_subsor
   Subroutine I_inssor (XDONT)
!  Sorts XDONT into increasing order (Insertion sort)
! __________________________________________________________
      Integer, dimension (:), Intent (InOut) :: XDONT
! __________________________________________________________
      Integer :: ICRS, IDCR
      Integer :: XWRK
!
      Do ICRS = 2, Size (XDONT)
         XWRK = XDONT (ICRS)
         If (XWRK >= XDONT(ICRS-1)) Cycle
         XDONT (ICRS) = XDONT (ICRS-1)
         Do IDCR = ICRS - 2, 1, - 1
            If (XWRK >= XDONT(IDCR)) Exit
            XDONT (IDCR+1) = XDONT (IDCR)
         End Do
         XDONT (IDCR+1) = XWRK
      End Do
!
      Return
!
End Subroutine I_inssor
!
end module m_refsor
!==============================================================================
Module m_unirnk
Integer, Parameter :: kdp = selected_real_kind(15)
public :: IXFunique_rank ! rename from unirnk
private :: kdp
private :: R_unirnk, I_unirnk, D_unirnk
private :: R_nearless, I_nearless, D_nearless, nearless
interface IXFunique_rank
  module procedure D_unirnk, R_unirnk, I_unirnk
end interface IXFunique_rank
interface nearless
  module procedure D_nearless, R_nearless, I_nearless
end interface nearless

contains

Subroutine D_unirnk (XVALT, IRNGT, NUNI)
! __________________________________________________________
!   UNIRNK = Merge-sort ranking of an array, with removal of
!   duplicate entries.
!   The routine is similar to pure merge-sort ranking, but on
!   the last pass, it discards indices that correspond to
!   duplicate entries.
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      Real (Kind=kdp), Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (Out) :: NUNI
! __________________________________________________________
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
      Real (Kind=kdp) :: XTST, XVALA, XVALB
!
!
      NVAL = Min (SIZE(XVALT), SIZE(IRNGT))
      NUNI = NVAL
!
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XVALT(IIND-1) < XVALT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 4) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XVALT(IRNGT(IWRKD+2)) <= XVALT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XVALT(IRNGT(IWRKD+1)) <= XVALT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XVALT(IRNGT(IWRKD+2)) <= XVALT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XVALT(IRNGT(IWRKD+1)) <= XVALT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XVALT(IRNG2) <= XVALT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XVALT(IRNG1) <= XVALT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XVALT(IRNG2) <= XVALT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (2*LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!  One steps in the C subset, that we create in the final rank array
!
!  Make a copy of the rank array for the iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
            XVALA = XVALT (JWRKT(IINDA))
            XVALB = XVALT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XVALT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XVALT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
!   Last merge of A and B into C, with removal of duplicates.
!
      IINDA = 1
      IINDB = LMTNA + 1
      NUNI = 0
!
!  One steps in the C subset, that we create in the final rank array
!
      JWRKT (1:LMTNA) = IRNGT (1:LMTNA)
      XTST = Nearless (Min(XVALT(JWRKT(1)), XVALT(IRNGT(IINDB))))
      Do IWRK = 1, NVAL
!
!  We still have unprocessed values in both A and B
!
         If (IINDA <= LMTNA) Then
            If (IINDB <= NVAL) Then
               If (XVALT(JWRKT(IINDA)) > XVALT(IRNGT(IINDB))) Then
                  IRNG = IRNGT (IINDB)
                  IINDB = IINDB + 1
               Else
                  IRNG = JWRKT (IINDA)
                  IINDA = IINDA + 1
               End If
            Else
!
!  Only A still with unprocessed values
!
               IRNG = JWRKT (IINDA)
               IINDA = IINDA + 1
            End If
         Else
!
!  Only B still with unprocessed values
!
            IRNG = IRNGT (IWRK)
         End If
         If (XVALT(IRNG) > XTST) Then
            XTST = XVALT (IRNG)
            NUNI = NUNI + 1
            IRNGT (NUNI) = IRNG
         End If
!
      End Do
!
      Return
!
End Subroutine D_unirnk

Subroutine R_unirnk (XVALT, IRNGT, NUNI)
! __________________________________________________________
!   UNIRNK = Merge-sort ranking of an array, with removal of
!   duplicate entries.
!   The routine is similar to pure merge-sort ranking, but on
!   the last pass, it discards indices that correspond to
!   duplicate entries.
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      Real, Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (Out) :: NUNI
! __________________________________________________________
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
      Real :: XTST, XVALA, XVALB
!
!
      NVAL = Min (SIZE(XVALT), SIZE(IRNGT))
      NUNI = NVAL
!
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XVALT(IIND-1) < XVALT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 4) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XVALT(IRNGT(IWRKD+2)) <= XVALT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XVALT(IRNGT(IWRKD+1)) <= XVALT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XVALT(IRNGT(IWRKD+2)) <= XVALT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XVALT(IRNGT(IWRKD+1)) <= XVALT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XVALT(IRNG2) <= XVALT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XVALT(IRNG1) <= XVALT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XVALT(IRNG2) <= XVALT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (2*LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!  One steps in the C subset, that we create in the final rank array
!
!  Make a copy of the rank array for the iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
            XVALA = XVALT (JWRKT(IINDA))
            XVALB = XVALT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XVALT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XVALT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
!   Last merge of A and B into C, with removal of duplicates.
!
      IINDA = 1
      IINDB = LMTNA + 1
      NUNI = 0
!
!  One steps in the C subset, that we create in the final rank array
!
      JWRKT (1:LMTNA) = IRNGT (1:LMTNA)
      XTST = Nearless (Min(XVALT(JWRKT(1)), XVALT(IRNGT(IINDB))))
      Do IWRK = 1, NVAL
!
!  We still have unprocessed values in both A and B
!
         If (IINDA <= LMTNA) Then
            If (IINDB <= NVAL) Then
               If (XVALT(JWRKT(IINDA)) > XVALT(IRNGT(IINDB))) Then
                  IRNG = IRNGT (IINDB)
                  IINDB = IINDB + 1
               Else
                  IRNG = JWRKT (IINDA)
                  IINDA = IINDA + 1
               End If
            Else
!
!  Only A still with unprocessed values
!
               IRNG = JWRKT (IINDA)
               IINDA = IINDA + 1
            End If
         Else
!
!  Only B still with unprocessed values
!
            IRNG = IRNGT (IWRK)
         End If
         If (XVALT(IRNG) > XTST) Then
            XTST = XVALT (IRNG)
            NUNI = NUNI + 1
            IRNGT (NUNI) = IRNG
         End If
!
      End Do
!
      Return
!
End Subroutine R_unirnk
Subroutine I_unirnk (XVALT, IRNGT, NUNI)
! __________________________________________________________
!   UNIRNK = Merge-sort ranking of an array, with removal of
!   duplicate entries.
!   The routine is similar to pure merge-sort ranking, but on
!   the last pass, it discards indices that correspond to
!   duplicate entries.
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      Integer, Dimension (:), Intent (In) :: XVALT
      Integer, Dimension (:), Intent (Out) :: IRNGT
      Integer, Intent (Out) :: NUNI
! __________________________________________________________
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
      Integer :: XTST, XVALA, XVALB
!
!
      NVAL = Min (SIZE(XVALT), SIZE(IRNGT))
      NUNI = NVAL
!
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XVALT(IIND-1) < XVALT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 4) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XVALT(IRNGT(IWRKD+2)) <= XVALT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XVALT(IRNGT(IWRKD+1)) <= XVALT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XVALT(IRNGT(IWRKD+2)) <= XVALT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XVALT(IRNGT(IWRKD+1)) <= XVALT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XVALT(IRNG2) <= XVALT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XVALT(IRNG1) <= XVALT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XVALT(IRNG2) <= XVALT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (2*LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!  One steps in the C subset, that we create in the final rank array
!
!  Make a copy of the rank array for the iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
            XVALA = XVALT (JWRKT(IINDA))
            XVALB = XVALT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XVALT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XVALT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
!   Last merge of A and B into C, with removal of duplicates.
!
      IINDA = 1
      IINDB = LMTNA + 1
      NUNI = 0
!
!  One steps in the C subset, that we create in the final rank array
!
      JWRKT (1:LMTNA) = IRNGT (1:LMTNA)
      XTST = Nearless (Min(XVALT(JWRKT(1)), XVALT(IRNGT(IINDB))))
      Do IWRK = 1, NVAL
!
!  We still have unprocessed values in both A and B
!
         If (IINDA <= LMTNA) Then
            If (IINDB <= NVAL) Then
               If (XVALT(JWRKT(IINDA)) > XVALT(IRNGT(IINDB))) Then
                  IRNG = IRNGT (IINDB)
                  IINDB = IINDB + 1
               Else
                  IRNG = JWRKT (IINDA)
                  IINDA = IINDA + 1
               End If
            Else
!
!  Only A still with unprocessed values
!
               IRNG = JWRKT (IINDA)
               IINDA = IINDA + 1
            End If
         Else
!
!  Only B still with unprocessed values
!
            IRNG = IRNGT (IWRK)
         End If
         If (XVALT(IRNG) > XTST) Then
            XTST = XVALT (IRNG)
            NUNI = NUNI + 1
            IRNGT (NUNI) = IRNG
         End If
!
      End Do
!
      Return
!
End Subroutine I_unirnk

Function D_nearless (XVAL) result (D_nl)
!  Nearest value less than given value
! __________________________________________________________
      Real (kind=kdp), Intent (In) :: XVAL
      Real (kind=kdp) :: D_nl
! __________________________________________________________
      D_nl = nearest (XVAL, -1.0_kdp)
      return
!
End Function D_nearless
Function R_nearless (XVAL) result (R_nl)
!  Nearest value less than given value
! __________________________________________________________
      Real, Intent (In) :: XVAL
      Real :: R_nl
! __________________________________________________________
      R_nl = nearest (XVAL, -1.0)
      return
!
End Function R_nearless
Function I_nearless (XVAL) result (I_nl)
!  Nearest value less than given value
! __________________________________________________________
      Integer, Intent (In) :: XVAL
      Integer :: I_nl
! __________________________________________________________
      I_nl = XVAL - 1
      return
!
End Function I_nearless

end module m_unirnk

!==============================================================================
Module m_mrgrnk
Integer, Parameter :: kdp = selected_real_kind(15)
public :: IXFrank ! rename from mrgrnk
private :: kdp
private :: R_mrgrnk, I_mrgrnk, D_mrgrnk
interface IXFrank
  module procedure D_mrgrnk, R_mrgrnk, I_mrgrnk
end interface IXFrank
contains

Subroutine D_mrgrnk (XDONT, IRNGT)
! __________________________________________________________
!   MRGRNK = Merge-sort ranking of an array
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      Real (kind=kdp), Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
      Real (kind=kdp) :: XVALA, XVALB
!
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XDONT), SIZE(IRNGT))
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XDONT(IIND-1) <= XDONT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 2) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B. This line may be activated when the
!   initial set is already close to sorted.
!
!          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
!
!  One steps in the C subset, that we build in the final rank array
!
!  Make a copy of the rank array for the merge iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
!
            XVALA = XDONT (JWRKT(IINDA))
            XVALB = XDONT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XDONT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XDONT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
      Return
!
End Subroutine D_mrgrnk

Subroutine R_mrgrnk (XDONT, IRNGT)
! __________________________________________________________
!   MRGRNK = Merge-sort ranking of an array
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! _________________________________________________________
      Real, Dimension (:), Intent (In) :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
      Real :: XVALA, XVALB
!
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XDONT), SIZE(IRNGT))
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XDONT(IIND-1) <= XDONT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 2) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B. This line may be activated when the
!   initial set is already close to sorted.
!
!          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
!
!  One steps in the C subset, that we build in the final rank array
!
!  Make a copy of the rank array for the merge iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
!
            XVALA = XDONT (JWRKT(IINDA))
            XVALB = XDONT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XDONT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XDONT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
      Return
!
End Subroutine R_mrgrnk
Subroutine I_mrgrnk (XDONT, IRNGT)
! __________________________________________________________
!   MRGRNK = Merge-sort ranking of an array
!   For performance reasons, the first 2 passes are taken
!   out of the standard loop, and use dedicated coding.
! __________________________________________________________
! __________________________________________________________
      Integer, Dimension (:), Intent (In)  :: XDONT
      Integer, Dimension (:), Intent (Out) :: IRNGT
! __________________________________________________________
      Integer :: XVALA, XVALB
!
      Integer, Dimension (SIZE(IRNGT)) :: JWRKT
      Integer :: LMTNA, LMTNC, IRNG1, IRNG2
      Integer :: NVAL, IIND, IWRKD, IWRK, IWRKF, JINDA, IINDA, IINDB
!
      NVAL = Min (SIZE(XDONT), SIZE(IRNGT))
      Select Case (NVAL)
      Case (:0)
         Return
      Case (1)
         IRNGT (1) = 1
         Return
      Case Default
         Continue
      End Select
!
!  Fill-in the index array, creating ordered couples
!
      Do IIND = 2, NVAL, 2
         If (XDONT(IIND-1) <= XDONT(IIND)) Then
            IRNGT (IIND-1) = IIND - 1
            IRNGT (IIND) = IIND
         Else
            IRNGT (IIND-1) = IIND
            IRNGT (IIND) = IIND - 1
         End If
      End Do
      If (Modulo(NVAL, 2) /= 0) Then
         IRNGT (NVAL) = NVAL
      End If
!
!  We will now have ordered subsets A - B - A - B - ...
!  and merge A and B couples into     C   -   C   - ...
!
      LMTNA = 2
      LMTNC = 4
!
!  First iteration. The length of the ordered subsets goes from 2 to 4
!
      Do
         If (NVAL <= 2) Exit
!
!   Loop on merges of A and B into C
!
         Do IWRKD = 0, NVAL - 1, 4
            If ((IWRKD+4) > NVAL) Then
               If ((IWRKD+2) >= NVAL) Exit
!
!   1 2 3
!
               If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Exit
!
!   1 3 2
!
               If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
                  IRNG2 = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNG2
!
!   3 1 2
!
               Else
                  IRNG1 = IRNGT (IWRKD+1)
                  IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+2)
                  IRNGT (IWRKD+2) = IRNG1
               End If
               Exit
            End If
!
!   1 2 3 4
!
            If (XDONT(IRNGT(IWRKD+2)) <= XDONT(IRNGT(IWRKD+3))) Cycle
!
!   1 3 x x
!
            If (XDONT(IRNGT(IWRKD+1)) <= XDONT(IRNGT(IWRKD+3))) Then
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+2) = IRNGT (IWRKD+3)
               If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   1 3 2 4
                  IRNGT (IWRKD+3) = IRNG2
               Else
!   1 3 4 2
                  IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+4) = IRNG2
               End If
!
!   3 x x x
!
            Else
               IRNG1 = IRNGT (IWRKD+1)
               IRNG2 = IRNGT (IWRKD+2)
               IRNGT (IWRKD+1) = IRNGT (IWRKD+3)
               If (XDONT(IRNG1) <= XDONT(IRNGT(IWRKD+4))) Then
                  IRNGT (IWRKD+2) = IRNG1
                  If (XDONT(IRNG2) <= XDONT(IRNGT(IWRKD+4))) Then
!   3 1 2 4
                     IRNGT (IWRKD+3) = IRNG2
                  Else
!   3 1 4 2
                     IRNGT (IWRKD+3) = IRNGT (IWRKD+4)
                     IRNGT (IWRKD+4) = IRNG2
                  End If
               Else
!   3 4 1 2
                  IRNGT (IWRKD+2) = IRNGT (IWRKD+4)
                  IRNGT (IWRKD+3) = IRNG1
                  IRNGT (IWRKD+4) = IRNG2
               End If
            End If
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 4
         Exit
      End Do
!
!  Iteration loop. Each time, the length of the ordered subsets
!  is doubled.
!
      Do
         If (LMTNA >= NVAL) Exit
         IWRKF = 0
         LMTNC = 2 * LMTNC
!
!   Loop on merges of A and B into C
!
         Do
            IWRK = IWRKF
            IWRKD = IWRKF + 1
            JINDA = IWRKF + LMTNA
            IWRKF = IWRKF + LMTNC
            If (IWRKF >= NVAL) Then
               If (JINDA >= NVAL) Exit
               IWRKF = NVAL
            End If
            IINDA = 1
            IINDB = JINDA + 1
!
!   Shortcut for the case when the max of A is smaller
!   than the min of B. This line may be activated when the
!   initial set is already close to sorted.
!
!          IF (XDONT(IRNGT(JINDA)) <= XDONT(IRNGT(IINDB))) CYCLE
!
!  One steps in the C subset, that we build in the final rank array
!
!  Make a copy of the rank array for the merge iteration
!
            JWRKT (1:LMTNA) = IRNGT (IWRKD:JINDA)
!
            XVALA = XDONT (JWRKT(IINDA))
            XVALB = XDONT (IRNGT(IINDB))
!
            Do
               IWRK = IWRK + 1
!
!  We still have unprocessed values in both A and B
!
               If (XVALA > XVALB) Then
                  IRNGT (IWRK) = IRNGT (IINDB)
                  IINDB = IINDB + 1
                  If (IINDB > IWRKF) Then
!  Only A still with unprocessed values
                     IRNGT (IWRK+1:IWRKF) = JWRKT (IINDA:LMTNA)
                     Exit
                  End If
                  XVALB = XDONT (IRNGT(IINDB))
               Else
                  IRNGT (IWRK) = JWRKT (IINDA)
                  IINDA = IINDA + 1
                  If (IINDA > LMTNA) Exit! Only B still with unprocessed values
                  XVALA = XDONT (JWRKT(IINDA))
               End If
!
            End Do
         End Do
!
!  The Cs become As and Bs
!
         LMTNA = 2 * LMTNA
      End Do
!
      Return
!
End Subroutine I_mrgrnk
end module m_mrgrnk

!==============================================================================
Module m_valmed
Integer, Parameter :: kdp = selected_real_kind(15)
public :: IXFmedian ! rename from valmed
private :: kdp
private :: R_valmed, I_valmed, D_valmed
interface IXFmedian
  module procedure d_valmed, r_valmed, i_valmed
end interface IXFmedian
contains

Recursive Function D_valmed (XDONT) Result (res_med)
!  Finds the median of XDONT using the recursive procedure
!  described in Knuth, The Art of Computer Programming,
!  vol. 3, 5.3.3 - This procedure is linear in time, and
!  does not require to be able to interpolate in the
!  set as the one used in INDNTH. It also has better worst
!  case behavior than INDNTH, but is about 30% slower in
!  average for random uniformly distributed values.
! __________________________________________________________
! __________________________________________________________
      Real (kind=kdp), Dimension (:), Intent (In) :: XDONT
      Real (kind=kdp) :: res_med
! __________________________________________________________
      Real (kind=kdp), Parameter :: XHUGE = HUGE (XDONT)
!      Real (kind=kdp), Dimension (SIZE(XDONT)+6) :: XWRKT
      Real (kind=kdp),allocatable, Dimension (:) :: XWRKT
      Real (kind=kdp) :: XWRK, XWRK1, XMED7
!      Integer, Dimension ((SIZE(XDONT)+6)/7) :: ISTRT, IENDT, IMEDT
      Integer,allocatable, Dimension (:) :: ISTRT, IENDT, IMEDT
      Integer :: NDON, NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, IWRK, IDCR, ICRS, ICRS1, ICRS2, IMED1
!
      NDON = SIZE (XDONT)
allocate(XWRKT(NDON+6),ISTRT((NDON+6)/7),IENDT((NDON+6)/7),IMEDT((NDON+6)/7))

      NMED = (NDON+1) / 2
!      write(unit=*,fmt=*) NMED, NDON
!
!  If the number of values is small, then use insertion sort
!
      If (NDON < 35) Then
!
!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDON
         If (XDONT (1) < XDONT (NDON)) Then
            XWRK = XDONT (1)
            XWRKT (IDCR) = XDONT (IDCR)
         Else
            XWRK = XDONT (IDCR)
            XWRKT (IDCR) = XDONT (1)
         Endif
         Do IWRK = 1, NDON - 2
            IDCR = IDCR - 1
            XWRK1 = XDONT (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (1) = XWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XWRKT (ICRS)
               IDCR = ICRS - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
            XWRKT (IDCR+1) = XWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         Do ICRS = NMED+1, NDON
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT (NMED)) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               XWRKT (IDCR+1) = XWRK
            End If
         End Do
         res_med = XWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  remove the loop test in the insertion loop.
!
      DO IDEB = 1, NDON-6, 7
         IDCR = IDEB + 6
         If (XDONT (IDEB) < XDONT (IDCR)) Then
            XWRK = XDONT (IDEB)
            XWRKT (IDCR) = XDONT (IDCR)
         Else
            XWRK = XDONT (IDCR)
            XWRKT (IDCR) = XDONT (IDEB)
         Endif
         Do IWRK = 1, 5
            IDCR = IDCR - 1
            XWRK1 = XDONT (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (IDEB) = XWRK
         Do ICRS = IDEB+2, IDEB+6
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT(ICRS-1)) Then
               XWRKT (ICRS) = XWRKT (ICRS-1)
               IDCR = ICRS - 1
               XWRK1 = XWRKT (IDCR-1)
               Do
                  If (XWRK >= XWRK1) Exit
                  XWRKT (IDCR) = XWRK1
                  IDCR = IDCR - 1
                  XWRK1 = XWRKT (IDCR-1)
               End Do
               XWRKT (IDCR) = XWRK
            EndIf
         End Do
      End Do
!
!  Add-up alternatively + and - HUGE values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDON/7)
      NTRI = NDON
      If (IDEB < NDON) Then
!
         XWRK1 = XHUGE
         Do ICRS = IDEB+1, IDEB+7
            If (ICRS <= NDON) Then
               XWRKT (ICRS) = XDONT (ICRS)
            Else
               If (XWRK1 /= XHUGE) NMED = NMED + 1
               XWRKT (ICRS) = XWRK1
               XWRK1 = - XWRK1
            Endif
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            XWRK = XWRKT (ICRS)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
         End Do
!
         NTRI = IDEB+7
      End If
!
!  Make the set of the indices of median values of each sorted subset
!
         IDON1 = 0
         Do IDON = 1, NTRI, 7
            IDON1 = IDON1 + 1
            IMEDT (IDON1) = IDON + 3
         End Do
!
!  Find XMED7, the median of the medians
!
         XMED7 = D_valmed (XWRKT (IMEDT))
!
!  Count how many values are not higher than (and how many equal to) XMED7
!  This number is at least 4 * 1/2 * (N/7) : 4 values in each of the
!  subsets where the median is lower than the median of medians. For similar
!  reasons, we also have at least 2N/7 values not lower than XMED7. At the
!  same time, we find in each subset the index of the last value < XMED7,
!  and that of the first > XMED7. These indices will be used to restrict the
!  search for the median as the Kth element in the subset (> or <) where
!  we know it to be.
!
         IDON1 = 1
         NLEQ = 0
         NEQU = 0
         Do IDON = 1, NTRI, 7
            IMED = IDON+3
            If (XWRKT (IMED) > XMED7) Then
                  IMED = IMED - 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XWRKT (IMED) < XMED7) Then
                  IMED = IMED + 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XWRKT (IMED) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XWRKT (IMED) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XWRKT (IMED) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XWRKT (IMED1) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XWRKT (IMED1) == XMED7) Then
                     NEQU = NEQU + 1
                     NLEQ = NLEQ + 1
                     ISTRT (IDON1) = IMED1 + 1
                  Else
                     Exit
                  End If
               End Do
            Endif
            IDON1 = IDON1 + 1
         End Do
!
!  Carry out a partial insertion sort to find the Kth smallest of the
!  large values, or the Kth largest of the small values, according to
!  what is needed.
!
        If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                XWRK1 = XHUGE
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
               Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) < XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                           If (ICRS2 < NORD) Then
                              XWRKT (ICRS1) = XWRKT (ICRS)
                              XWRK1 = XWRKT(ICRS1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) >= XWRK1) Exit
                         XWRK = XWRKT (ICRS)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                         End Do
                         XWRKT (IDCR+1) = XWRK
                         XWRK1 = XWRKT(ICRS1)
                      End Do
                   End If
                End Do
                res_med = XWRK1
                Return
            Else
                res_med = XMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = -XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            If (ICRS2 < NORD) Then
                               XWRKT (ICRS1) = XWRKT (ICRS)
                               XWRK1 = XWRKT (ICRS1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            Exit
                         End If
                      End Do
                   Endif
                End Do
!
                res_med = XWRK1
                Return
         End If
!
End Function D_valmed

Recursive Function R_valmed (XDONT) Result (res_med)
!  Finds the median of XDONT using the recursive procedure
!  described in Knuth, The Art of Computer Programming,
!  vol. 3, 5.3.3 - This procedure is linear in time, and
!  does not require to be able to interpolate in the
!  set as the one used in INDNTH. It also has better worst
!  case behavior than INDNTH, but is about 30% slower in
!  average for random uniformly distributed values.
! __________________________________________________________
! _________________________________________________________
      Real, Dimension (:), Intent (In) :: XDONT
      Real :: res_med
! __________________________________________________________
      Real, Parameter :: XHUGE = HUGE (XDONT)
!      Real, Dimension (SIZE(XDONT)+6) :: XWRKT     
      Real ,allocatable, Dimension (:) :: XWRKT
      Real :: XWRK, XWRK1, XMED7
!
!      Integer, Dimension ((SIZE(XDONT)+6)/7) :: ISTRT, IENDT, IMEDT
      Integer,allocatable, Dimension (:) :: ISTRT, IENDT, IMEDT
 
      Integer :: NDON, NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, IWRK, IDCR, ICRS, ICRS1, ICRS2, IMED1
!
      NDON = SIZE (XDONT)
allocate(XWRKT(NDON+6),ISTRT((NDON+6)/7),IENDT((NDON+6)/7),IMEDT((NDON+6)/7))
      NMED = (NDON+1) / 2
!      write(unit=*,fmt=*) NMED, NDON
!
!  If the number of values is small, then use insertion sort
!
      If (NDON < 35) Then
!

!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDON
         If (XDONT (1) < XDONT (NDON)) Then
            XWRK = XDONT (1)
            XWRKT (IDCR) = XDONT (IDCR)
         Else
            XWRK = XDONT (IDCR)
            XWRKT (IDCR) = XDONT (1)
         Endif
         Do IWRK = 1, NDON - 2
            IDCR = IDCR - 1
            XWRK1 = XDONT (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (1) = XWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XWRKT (ICRS)
               IDCR = ICRS - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
            XWRKT (IDCR+1) = XWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         Do ICRS = NMED+1, NDON
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT (NMED)) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               XWRKT (IDCR+1) = XWRK
            End If
         End Do
         res_med = XWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  remove the loop test in the insertion loop.
!
      DO IDEB = 1, NDON-6, 7
         IDCR = IDEB + 6
         If (XDONT (IDEB) < XDONT (IDCR)) Then
            XWRK = XDONT (IDEB)
            XWRKT (IDCR) = XDONT (IDCR)
         Else
            XWRK = XDONT (IDCR)
            XWRKT (IDCR) = XDONT (IDEB)
         Endif
         Do IWRK = 1, 5
            IDCR = IDCR - 1
            XWRK1 = XDONT (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (IDEB) = XWRK
         Do ICRS = IDEB+2, IDEB+6
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT(ICRS-1)) Then
               XWRKT (ICRS) = XWRKT (ICRS-1)
               IDCR = ICRS - 1
               XWRK1 = XWRKT (IDCR-1)
               Do
                  If (XWRK >= XWRK1) Exit
                  XWRKT (IDCR) = XWRK1
                  IDCR = IDCR - 1
                  XWRK1 = XWRKT (IDCR-1)
               End Do
               XWRKT (IDCR) = XWRK
            EndIf
         End Do
      End Do
!
!  Add-up alternatively + and - HUGE values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDON/7)
      NTRI = NDON
      If (IDEB < NDON) Then
!
         XWRK1 = XHUGE
         Do ICRS = IDEB+1, IDEB+7
            If (ICRS <= NDON) Then
               XWRKT (ICRS) = XDONT (ICRS)
            Else
               If (XWRK1 /= XHUGE) NMED = NMED + 1
               XWRKT (ICRS) = XWRK1
               XWRK1 = - XWRK1
            Endif
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            XWRK = XWRKT (ICRS)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
         End Do
!
         NTRI = IDEB+7
      End If
!
!  Make the set of the indices of median values of each sorted subset
!
         IDON1 = 0
         Do IDON = 1, NTRI, 7
            IDON1 = IDON1 + 1
            IMEDT (IDON1) = IDON + 3
         End Do
!
!  Find XMED7, the median of the medians
!
         XMED7 = R_valmed (XWRKT (IMEDT))
!
!  Count how many values are not higher than (and how many equal to) XMED7
!  This number is at least 4 * 1/2 * (N/7) : 4 values in each of the
!  subsets where the median is lower than the median of medians. For similar
!  reasons, we also have at least 2N/7 values not lower than XMED7. At the
!  same time, we find in each subset the index of the last value < XMED7,
!  and that of the first > XMED7. These indices will be used to restrict the
!  search for the median as the Kth element in the subset (> or <) where
!  we know it to be.
!
         IDON1 = 1
         NLEQ = 0
         NEQU = 0
         Do IDON = 1, NTRI, 7
            IMED = IDON+3
            If (XWRKT (IMED) > XMED7) Then
                  IMED = IMED - 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XWRKT (IMED) < XMED7) Then
                  IMED = IMED + 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XWRKT (IMED) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XWRKT (IMED) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XWRKT (IMED) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XWRKT (IMED1) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XWRKT (IMED1) == XMED7) Then
                     NEQU = NEQU + 1
                     NLEQ = NLEQ + 1
                     ISTRT (IDON1) = IMED1 + 1
                  Else
                     Exit
                  End If
               End Do
            Endif
            IDON1 = IDON1 + 1
         End Do
!
!  Carry out a partial insertion sort to find the Kth smallest of the
!  large values, or the Kth largest of the small values, according to
!  what is needed.
!
        If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                XWRK1 = XHUGE
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
               Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) < XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                           If (ICRS2 < NORD) Then
                              XWRKT (ICRS1) = XWRKT (ICRS)
                              XWRK1 = XWRKT(ICRS1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) >= XWRK1) Exit
                         XWRK = XWRKT (ICRS)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                         End Do
                         XWRKT (IDCR+1) = XWRK
                         XWRK1 = XWRKT(ICRS1)
                      End Do
                   End If
                End Do
                res_med = XWRK1
                Return
            Else
                res_med = XMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = -XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            If (ICRS2 < NORD) Then
                               XWRKT (ICRS1) = XWRKT (ICRS)
                               XWRK1 = XWRKT (ICRS1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            Exit
                         End If
                      End Do
                   Endif
                End Do
!
                res_med = XWRK1
                Return
         End If
!
End Function R_valmed
Recursive Function I_valmed (XDONT) Result (res_med)
!  Finds the median of XDONT using the recursive procedure
!  described in Knuth, The Art of Computer Programming,
!  vol. 3, 5.3.3 - This procedure is linear in time, and
!  does not require to be able to interpolate in the
!  set as the one used in INDNTH. It also has better worst
!  case behavior than INDNTH, but is about 30% slower in
!  average for random uniformly distributed values.
! __________________________________________________________
! __________________________________________________________
      Integer, Dimension (:), Intent (In)  :: XDONT
      Integer :: res_med
! __________________________________________________________
      Integer, Parameter :: XHUGE = HUGE (XDONT)
!      Integer, Dimension (SIZE(XDONT)+6) :: XWRKT
      Integer,allocatable, Dimension (:) :: XWRKT     
      Integer :: XWRK, XWRK1, XMED7
!
!      Integer, Dimension ((SIZE(XDONT)+6)/7) :: ISTRT, IENDT, IMEDT
      Integer,allocatable, Dimension (:) :: ISTRT, IENDT, IMEDT
      Integer :: NDON, NTRI, NMED, NORD, NEQU, NLEQ, IMED, IDON, IDON1
      Integer :: IDEB, IWRK, IDCR, ICRS, ICRS1, ICRS2, IMED1
!
      NDON = SIZE (XDONT)
allocate(XWRKT(NDON+6),ISTRT((NDON+6)/7),IENDT((NDON+6)/7),IMEDT((NDON+6)/7))      
      NMED = (NDON+1) / 2
!      write(unit=*,fmt=*) NMED, NDON
!
!  If the number of values is small, then use insertion sort
!
      If (NDON < 35) Then
!
!  Bring minimum to first location to save test in decreasing loop
!
         IDCR = NDON
         If (XDONT (1) < XDONT (NDON)) Then
            XWRK = XDONT (1)
            XWRKT (IDCR) = XDONT (IDCR)
         Else
            XWRK = XDONT (IDCR)
            XWRKT (IDCR) = XDONT (1)
         Endif
         Do IWRK = 1, NDON - 2
            IDCR = IDCR - 1
            XWRK1 = XDONT (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (1) = XWRK
!
! Sort the first half, until we have NMED sorted values
!
         Do ICRS = 3, NMED
            XWRK = XWRKT (ICRS)
               IDCR = ICRS - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
            XWRKT (IDCR+1) = XWRK
         End Do
!
!  Insert any value less than the current median in the first half
!
         Do ICRS = NMED+1, NDON
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT (NMED)) Then
               IDCR = NMED - 1
               Do
                  If (XWRK >= XWRKT(IDCR)) Exit
                  XWRKT (IDCR+1) = XWRKT (IDCR)
                  IDCR = IDCR - 1
               End Do
               XWRKT (IDCR+1) = XWRK
            End If
         End Do
         res_med = XWRKT (NMED)
         Return
      End If
!
!  Make sorted subsets of 7 elements
!  This is done by a variant of insertion sort where a first
!  pass is used to bring the smallest element to the first position
!  decreasing disorder at the same time, so that we may remove
!  remove the loop test in the insertion loop.
!
      DO IDEB = 1, NDON-6, 7
         IDCR = IDEB + 6
         If (XDONT (IDEB) < XDONT (IDCR)) Then
            XWRK = XDONT (IDEB)
            XWRKT (IDCR) = XDONT (IDCR)
         Else
            XWRK = XDONT (IDCR)
            XWRKT (IDCR) = XDONT (IDEB)
         Endif
         Do IWRK = 1, 5
            IDCR = IDCR - 1
            XWRK1 = XDONT (IDCR)
            If (XWRK1 < XWRK) Then
                XWRKT (IDCR) = XWRK
                XWRK = XWRK1
            Else
                XWRKT (IDCR) = XWRK1
            Endif
         End Do
         XWRKT (IDEB) = XWRK
         Do ICRS = IDEB+2, IDEB+6
            XWRK = XWRKT (ICRS)
            If (XWRK < XWRKT(ICRS-1)) Then
               XWRKT (ICRS) = XWRKT (ICRS-1)
               IDCR = ICRS - 1
               XWRK1 = XWRKT (IDCR-1)
               Do
                  If (XWRK >= XWRK1) Exit
                  XWRKT (IDCR) = XWRK1
                  IDCR = IDCR - 1
                  XWRK1 = XWRKT (IDCR-1)
               End Do
               XWRKT (IDCR) = XWRK
            EndIf
         End Do
      End Do
!
!  Add-up alternatively + and - HUGE values to make the number of data
!  an exact multiple of 7.
!
      IDEB = 7 * (NDON/7)
      NTRI = NDON
      If (IDEB < NDON) Then
!
         XWRK1 = XHUGE
         Do ICRS = IDEB+1, IDEB+7
            If (ICRS <= NDON) Then
               XWRKT (ICRS) = XDONT (ICRS)
            Else
               If (XWRK1 /= XHUGE) NMED = NMED + 1
               XWRKT (ICRS) = XWRK1
               XWRK1 = - XWRK1
            Endif
         End Do
!
         Do ICRS = IDEB+2, IDEB+7
            XWRK = XWRKT (ICRS)
            Do IDCR = ICRS - 1, IDEB+1, - 1
               If (XWRK >= XWRKT(IDCR)) Exit
               XWRKT (IDCR+1) = XWRKT (IDCR)
            End Do
            XWRKT (IDCR+1) = XWRK
         End Do
!
         NTRI = IDEB+7
      End If
!
!  Make the set of the indices of median values of each sorted subset
!
         IDON1 = 0
         Do IDON = 1, NTRI, 7
            IDON1 = IDON1 + 1
            IMEDT (IDON1) = IDON + 3
         End Do
!
!  Find XMED7, the median of the medians
!
         XMED7 = I_valmed (XWRKT (IMEDT))
!
!  Count how many values are not higher than (and how many equal to) XMED7
!  This number is at least 4 * 1/2 * (N/7) : 4 values in each of the
!  subsets where the median is lower than the median of medians. For similar
!  reasons, we also have at least 2N/7 values not lower than XMED7. At the
!  same time, we find in each subset the index of the last value < XMED7,
!  and that of the first > XMED7. These indices will be used to restrict the
!  search for the median as the Kth element in the subset (> or <) where
!  we know it to be.
!
         IDON1 = 1
         NLEQ = 0
         NEQU = 0
         Do IDON = 1, NTRI, 7
            IMED = IDON+3
            If (XWRKT (IMED) > XMED7) Then
                  IMED = IMED - 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Else If (XWRKT (IMED) < XMED7) Then
                  IMED = IMED + 2
                  If (XWRKT (IMED) > XMED7) Then
                     IMED = IMED - 1
                  Else If (XWRKT (IMED) < XMED7) Then
                     IMED = IMED + 1
                  Endif
            Endif
            If (XWRKT (IMED) > XMED7) Then
               NLEQ = NLEQ + IMED - IDON
               IENDT (IDON1) = IMED - 1
               ISTRT (IDON1) = IMED
            Else If (XWRKT (IMED) < XMED7) Then
               NLEQ = NLEQ + IMED - IDON + 1
               IENDT (IDON1) = IMED
               ISTRT (IDON1) = IMED + 1
            Else                    !       If (XWRKT (IMED) == XMED7)
               NLEQ = NLEQ + IMED - IDON + 1
               NEQU = NEQU + 1
               IENDT (IDON1) = IMED - 1
               Do IMED1 = IMED - 1, IDON, -1
                  If (XWRKT (IMED1) == XMED7) Then
                     NEQU = NEQU + 1
                     IENDT (IDON1) = IMED1 - 1
                  Else
                     Exit
                  End If
               End Do
               ISTRT (IDON1) = IMED + 1
               Do IMED1 = IMED + 1, IDON + 6
                  If (XWRKT (IMED1) == XMED7) Then
                     NEQU = NEQU + 1
                     NLEQ = NLEQ + 1
                     ISTRT (IDON1) = IMED1 + 1
                  Else
                     Exit
                  End If
               End Do
            Endif
            IDON1 = IDON1 + 1
         End Do
!
!  Carry out a partial insertion sort to find the Kth smallest of the
!  large values, or the Kth largest of the small values, according to
!  what is needed.
!
        If (NLEQ - NEQU + 1 <= NMED) Then
            If (NLEQ < NMED) Then   !      Not enough low values
                XWRK1 = XHUGE
                NORD = NMED - NLEQ
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                IDCR = 0
               Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) < XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                           If (ICRS2 < NORD) Then
                              XWRKT (ICRS1) = XWRKT (ICRS)
                              XWRK1 = XWRKT(ICRS1)
                           Endif
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = ISTRT (IDON1), IDON + 6
                         If (XWRKT(ICRS) >= XWRK1) Exit
                         XWRK = XWRKT (ICRS)
                         Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK >= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                         End Do
                         XWRKT (IDCR+1) = XWRK
                         XWRK1 = XWRKT(ICRS1)
                      End Do
                   End If
                End Do
                res_med = XWRK1
                Return
            Else
                res_med = XMED7
                Return
            End If
         Else                       !      If (NLEQ > NMED)
!                                          Not enough high values
                XWRK1 = -XHUGE
                NORD = NLEQ - NEQU - NMED + 1
                IDON1 = 0
                ICRS1 = 1
                ICRS2 = 0
                Do IDON = 1, NTRI, 7
                   IDON1 = IDON1 + 1
                   If (ICRS2 < NORD) Then
!
                      Do ICRS = IDON, IENDT (IDON1)
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            If (ICRS2 < NORD) Then
                               XWRKT (ICRS1) = XWRKT (ICRS)
                               XWRK1 = XWRKT (ICRS1)
                            End If
                         End If
                         ICRS1 = MIN (NORD, ICRS1 + 1)
                         ICRS2 = MIN (NORD, ICRS2 + 1)
                      End Do
                   Else
                      Do ICRS = IENDT (IDON1), IDON, -1
                         If (XWRKT(ICRS) > XWRK1) Then
                            XWRK = XWRKT (ICRS)
                            IDCR = ICRS1 - 1
                            Do IDCR = ICRS1 - 1, 1, - 1
                               If (XWRK <= XWRKT(IDCR)) Exit
                               XWRKT (IDCR+1) = XWRKT (IDCR)
                            End Do
                            XWRKT (IDCR+1) = XWRK
                            XWRK1 = XWRKT(ICRS1)
                         Else
                            Exit
                         End If
                      End Do
                   Endif
                End Do
!
                res_med = XWRK1
                Return
         End If
!
End Function I_valmed
end module m_valmed

!==============================================================================
Module IXMsort    
    use m_refsor
    use m_mrgrnk
    use m_valmed
    use m_unirnk
end module IXMsort
