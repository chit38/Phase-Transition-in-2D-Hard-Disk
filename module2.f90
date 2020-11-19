module gr
use variables
implicit none

contains
subroutine calculate_gr(T)
integer, intent(in) :: T
integer :: maxiter, i, j, k, x, y, z, nij = 0
real*8 :: r = 0, d(2), rjk, r_lower, r_upper
real*8, allocatable :: dum(:), g(:)

maxiter = int((L/2 )/0.1d0)
print *, maxiter
allocate(dum(maxiter))
allocate(g(maxiter))
do z = 1, maxiter
do y = 1, T
do j = 1, 63
    do k = j+1,64
            d(1:2) = xyz(y, j, 1:2) - xyz(y, k, 1:2)
            d(1:2) = d(1:2) - L*anint(d(1:2)/L)
            rjk = dot_product(d(1:2), d(1:2))
            rjk = sqrt(rjk)
            if((rjk .ge. r) .and. (rjk .le. r+0.1d0))nij = nij + 1
    end do
end do
end do
!print *, nij
g(z) = nij/(2*pi*r*0.1d0*63*T*rho)
nij = 0
r = r + 0.1d0
end do
gsigma = g(11)
r = 0
open(1, file="hist.txt", status="old", form='formatted')
do x = 1, maxiter
    write(1,*)r+(x-1)*0.1d0, g(x)
end do
call execute_command_line('gnuplot -p plot.plt')

end subroutine calculate_gr

subroutine calculate_betap(betap)
real*8 ,intent(out) ::  betap
betap = 1 + 2*pi*(1.d0/3)*rho*gsigma
end subroutine calculate_betap

end module gr