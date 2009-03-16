$!=============================================
$! g2a.com  Convert Genie files to ascii format
$!============================================
$! -- for IRIS PG002 25 Hz 4000 tof points --
$!> set work 47 4010
$! -- for IRIS PG002 50 Hz 2000 tof points -- 
> set work 60 2010 
$ inquire p1 " Run number of .ipg/.iag file  "
$! inquire p2 " Energy binning : Emin         "
$! inquire p4 "                  Emax         "
$! inquire p3 "                  dE           "   
$ ext="ipg"
$ area="scratch$disk:[iris]"    
$ p2="-0.2"
$ p3="0.005"
$ p4="1.6"
$ do i=1,51
> @i_p:r_int irs 'p1' 1 'i' 'ext' 'area'    
> rebin w1 'p2' ('p3') 'p4'
> show data w1 /out='area'irs'p1'.'i'
$ end do
