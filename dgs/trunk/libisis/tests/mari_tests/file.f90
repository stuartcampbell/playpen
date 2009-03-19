program filemaker
implicit none
integer::wkno,i

wkno=285


write(66,*)wkno
do i=27,63
write(66,*)i
write(66,*)'3'
write(66,*)117+i,378+i,637+i
write(66,*)' '
enddo


write(66,*)'181'
write(66,*)'1'
write(66,*)'181'
write(66,*)' '


write(66,*)'182'
write(66,*)'1'
write(66,*)'182'
write(66,*)' '

do i=1,222
write(66,*)182+i
write(66,*)'3'
write(66,*)182+i,441+i,700+i
write(66,*)' '
enddo


end
