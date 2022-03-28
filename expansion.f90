program expansion 
!Miguel Sabogal Garcia
implicit none 

!Variables y parametros
integer, parameter :: N=200
integer, parameter :: max_dias=1000
integer :: i,j,k1,k2,nt,dias,ni
integer, dimension(N,N) :: pob, pob_aux 
integer, dimension(N,N) :: inm, inm_aux 
integer, dimension(0:N+1) :: fro
real :: x,nivel

open(1,file='expansion.dat')
open(2,file='enfermos.dat')
open(3,file='inmunes.dat')

!Inicializaciones
x=0.
pob(:,:)=0
inm(:,:)=0
i = N/2
pob(i,i) = 1

!Condiciones de frontera periodicas 
fro(0)= N
        do i=1,N
                fro(i) = i
        enddo
fro(N+1) =1
dias=0
call random_seed

!Proceso de contagio 
do while (dias<max_dias)

pob_aux(:,:)= pob(:,:)
inm_aux(:,:)= inm(:,:)

        do i=1,N
                do j=1,N
                        if(pob_aux(i,j)==0.and.inm_aux(i,j)==0) then
                                !K es numero de enfermos alrededor kmin=0 kmax=4
                                k1= pob_aux(fro(i+1),fro(j)) + pob_aux(fro(i-1),fro(j)) + pob_aux(fro(i),fro(j+1)) + pob_aux(fro(i),fro(j-1)) !Primeros vecinos 
                                k2= pob_aux(fro(i+1),fro(j+1)) + pob_aux(fro(i-1),fro(j+1)) + pob_aux(fro(i+1),fro(j-1)) + pob_aux(fro(i-1),fro(j-1)) !Segundos vecinos 

                                call random_number(x)
                                nivel = 0.1*k1 + 0.05*k2

                                if(x < nivel) then
                                        call random_number(x)
                                        if(x<0.65) then
                                                inm(i,j) = 1
                                        else 
                                                pob(i,j) = 1  
                                        endif        
                                endif
                        endif  
                enddo
        enddo 
        nt= sum(pob)
        ni= sum(inm)
        dias= dias + 1
        write(1,*), dias, nt, ni
        print *, dias, nt,ni
enddo

do i=1,N
    do j=1,N

        write(2,999) i,j,pob(i,j) 
        write(3,999) i,j,inm(i,j)
    
    enddo

    write(2,*)
    write(3,*)
enddo


close(1)
close(2)
close(3)

999 format(3(I8,1X))
end program expansion 
