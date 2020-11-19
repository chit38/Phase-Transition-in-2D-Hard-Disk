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
allocate(grid(64,2))
do i = 1,8 
    do j = 1, 8
        index =  8*(i-1) + j
        grid(index, 1) = (i-1)*(real(L)/real(7))
        grid(index, 2) = (j-1)*(real(L)/real(7))
    end do
end do
end subroutine create_lattice



subroutine mc_step(ans)
integer, intent(out) :: ans
real*8 :: rand, dum(2), a(2), r(2), rij
integer :: index, i

ans = 1
call random_number(rand)
index = ceiling(rand*64)

call random_number(rand)
a = rand*2.d0 - 1.d0

dum(1:2) = grid(index, 1:2) + a(1:2)*step

do i = 1, 64
    r(1:2) = dum(1:2) - grid(i, 1:2)
    r(1:2) = r(1:2) - L*anint(r(1:2)/L)
    rij = dot_product(r(1:2), r(1:2))
    rij = sqrt(rij)
    if(rij .le. sigma .and. i/=index)ans = 0
end do
!print *, dum
if(ans==1)then
grid(index, 1:2) = dum(1:2)
grid(index,1:2) = grid(index,1:2) - L*floor(grid(index,1:2)/L)
!call printxyz()
end if 
end subroutine mc_step



subroutine printxyz()
integer :: i,j
write(21,*)64
write(21,*)"cc"
do i = 1, 8
    do j = 1, 8
        write(21,*)"H", grid((i-1)*8+j,1:2), 0.d0
    end do
end do
write(21,*)
end subroutine printxyz

end module mc