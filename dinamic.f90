module pob
integer :: n
integer, dimension(:), allocatable :: poblacion

end module pob 


program dinamic 

use pob
implicit none

integer , parameter :: ni=100

integer, parameter :: rp=3

integer, parameter :: ng=100

integer :: np

integer :: k

n=ni

do k = 1,ng

  allocate(poblacion(n))

  call generador_de_poblacion

  call numero_de_parejas(np)

  if(np==0) then

    print *, 'la poblacion colapso'

    stop

  endif

  n = np*rp ! aqui definimos el nuevo numero de individuos

  deallocate(poblacion)

  print *, k,n

enddo

end program dinamic



integer, dimension (N) :: vec_fix

integer, dimension(:), allocatable :: vec_din


allocate(vec_din(N))

call operacion(vec_din)

do i=1,N

        print*, vec_din(i)

enddo
end program dinamic 

subroutine operacion(vec_din)
use pob

implicit none 

integer :: i,j,N

N= size(vec_din)

do i=1,N

vec_din(i) = 2*i**2 -10

enddo 


end subroutine operacion   
