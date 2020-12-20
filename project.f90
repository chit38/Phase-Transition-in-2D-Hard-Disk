program project
use variables
use mc
use gr

implicit none
integer :: i, acccepted = 0, flag, nstep, j, k=1
real*8 :: betap

call create_lattice()

print *, "Enter stpesize and number of steps"
read(*,*)step, nstep

allocate(xyz(nstep,64,2))

open(21,file="traj.xyz",status='new',form='formatted')
close(21)

do i = 1, nstep
   call mc_step(flag)
   acccepted = acccepted + flag
   if(flag==1)then
      call printxyz()
   end if
   if(mod(i, 100) == 0)then
   print *, "steps:", i, "Accepted percentage:", 100*real(acccepted)/real(i)
   end if
end do

print*, "naccept=",acccepted
call calculate_gr(acccepted)

end program project
