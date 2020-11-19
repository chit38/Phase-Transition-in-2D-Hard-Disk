module variables
implicit none
real*8, allocatable :: grid(:,:), hist(:,:), xyz(:,:,:)
integer, allocatable :: randomnum(:,:)
real*8 :: sigma, rho, L, step, gsigma
real*8, parameter :: PI=4.D0*DATAN(1.D0)
contains
end module variables