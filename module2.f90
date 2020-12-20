module gr
use variables
implicit none

contains
subroutine calculate_gr(T)
integer, intent(in) :: T
integer :: maxiter, i, j, k, y, bin,nat
real*8 :: r = 0, d(2), rjk, zero, gsigma, betap
real*8, allocatable :: dum(:), g(:)
character(len=2) :: comment,at

maxiter = int((L/2)/0.1d0)
print *, maxiter
allocate(dum(maxiter))
allocate(g(maxiter))
dum(1:maxiter) = 0.d0

open(unit=22,file="test094.xyz",status='unknown')
 do i = 1,T
    read(22,*) nat 
    read(22,*) comment
    do j = 1,nat
       read(22,*) at, xyz(i,j,1:2),zero
    enddo
enddo


do y = 1, T
   do j = 1, 63
      do k = j+1,64
            d(1:2) = xyz(y, k, 1:2) - xyz(y, j, 1:2)
            d(1:2) = d(1:2) - L*nint(d(1:2)/L)
            rjk = dot_product(d(1:2), d(1:2))
            rjk = sqrt(rjk)
            if(rjk < 0.5d0*L) then
            bin = int(rjk/0.1d0)
            if (bin > maxiter) exit
            dum(bin) = dum(bin) + 1.d0
            write(11,*) bin
            endif
      end do
   end do
end do


open(1, file="hist.txt", status="unknown", form='formatted')
do bin = 1, maxiter-1
    r = DFLOAT(bin)*0.1d0
    g(bin) = dum(bin)/(pi*r*0.1d0*63*T*rho)
    write(1,*)r, g(bin)
end do

gsigma = g(10)
betap = 1 + 2.d0*PI*rho*gsigma/3.d0
print "(A15, F10.6)", "g(sigma)=", gsigma
print "(A15, F10.6)", "beta*p/rho = ", betap

end subroutine calculate_gr
end module gr
