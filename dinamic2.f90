program dinamic 

implicit none 

integer , parameter :: N=100
integer :: i,j,k

integer, dimension (N) :: vec_fix

integer, dimension(:), allocatable :: vec_din

interface 

        subroutine operacion(vec_din)
        implicit none 

        integer , dimension(:), allocatable, intent(inout) :: vec_din

        end subroutine operacion

end interface        

allocate(vec_din(N))

call operacion(vec_din)

do i=1,N

        print*, vec_din(i)

enddo
end program dinamic 

subroutine operacion(vec_din)
implicit none 

integer , dimension(:), allocatable, intent(inout)  :: vec_din
integer :: i,j,N

N= size(vec_din)

do i=1,N

vec_din(i) = 2*i**2 -10

enddo 


end subroutine operacion   
