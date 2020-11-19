program project
use variables
use mc
use gr
implicit none
integer :: i, acccepted = 0, flag, nstep, j, k=1
real*8 :: betap
call create_lattice()
!call printxyz()
print *, "Enter stpesize and number of steps"
read(*,*)step, nstep
!call random_generator(nstep)
!grans = 0
allocate(xyz(nstep,64,2))
do i = 1, nstep
   call mc_step(flag)
   !print *, flag
   acccepted = acccepted + flag
   if(flag==1)then
      do j = 1,64
      xyz(acccepted, j, 1:2) = grid(j, 1:2)
      end do
      call printxyz()
   end if
   if(mod(i, 100) == 0)then
   print *, "steps:", i, "Accepted percentage:", 100*real(acccepted)/real(i)
   end if
  
end do
print*, "naccept=",acccepted
!call plot_gr()
call calculate_gr(acccepted)
!grans = grans/acccepted
!print *, grans
!call calculate_betap(betap)
!print *, betap
end program project
