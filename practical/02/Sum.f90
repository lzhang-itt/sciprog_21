
program sum
! Declare variables
   integer (kind=4) :: i
   real (kind=4) :: sum1, sum2, diff
   

! First sum
   sum1 = 0.0
   do i=1,1000
      sum1 = sum1 + real(1.0 / i)
   end do


! Second sum 
   sum2 = 0.0
   do i=1000,1,-1
      sum2 = sum2 + real(1.0 / i)
   end do

   write(6,*) ' Sum1=',sum1
   write(6,*) ' Sum2=',sum2

! Find the difference
   diff = sum1 - sum2

   write(6,*) ' Difference between the two is ',diff
   ! mathematically the same but errors accumulate differently

end program sum
