module mc
use variables
implicit none
contains
subroutine create_lattice()
integer :: n, i , j, index
print *, "Please enter density"
read(*,*)rho
n = 64
L = sqrt(real(n)/rho)
sigma = 1.d0
print *, L
allocate(grid(n,2))
do i = 1,8 
    do j = 1, 8
        index =  8*(i-1) + j
        grid(index, 1) =  (i-1)*(real(L)/real(8))
        grid(index, 2) =  (j-1)*(real(L)/real(8))
    end do
end do
call printxyz()
end subroutine create_lattice



subroutine mc_step(ans)
integer, intent(out) :: ans
real*8 :: rand, dum(2), a(2), r(2), rij,rand2(2)
integer :: index, i

ans = 1
call random_number(rand)
index = ceiling(rand*64)

call random_number(rand2)
a = rand2*2.d0 - 1.d0
write(10,*) "a=", a
dum(1:2) = grid(index, 1:2) + a(1:2)*step

dum(1:2) = dum(1:2) - L*floor(dum(1:2)/L)

do i = 1, 64
	if (i .ne. index) then
    r(1:2) = dum(1:2) - grid(i,1:2)
    r(1:2) = r(1:2) - L*nint(r(1:2)/L)
    rij = dot_product(r(1:2), r(1:2))
    rij = sqrt(rij)
    if(rij .lt. sigma ) ans = 0
    endif
end do
!print *, dum
if(ans==1)then
grid(index, 1:2) = dum(1:2)
!grid(index,1:2) = grid(index,1:2) - L*nint(grid(index,1:2)/L)
end if 
end subroutine mc_step



subroutine printxyz()
integer :: i,j
open(21,file="traj.xyz",status='unknown',form='formatted', ACCESS='APPEND')
write(21,*)64
write(21,*)"cc"
do i = 1, 8
    do j = 1, 8
        write(21,'(a2,3f15.6)') "H", grid((i-1)*8+j,1:2), 0.d0
    end do
end do
close(21)
end subroutine printxyz

end module mc
