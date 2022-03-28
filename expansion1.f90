
program expansion 
implicit none 

integer, parameter :: N=100
integer, parameter :: max_dias=300
integer :: i,j,k,nt,dias
integer, dimension(N,N) :: pob, pob_aux 
integer, dimension(0:N+1) :: fro
real :: x,nivel


x=0.
pob(:,:)=0
i = N/2
pob(i,i) = 1
!pob_aux(:,:)= pob(:,:)
!Condiciones de frontera periodicas 

fro(0)= N

do i=1,N
        fro(i) = i
enddo

fro(N+1) =1

dias=0
call random_seed

do while (dias<max_dias)
pob_aux(:,:)= pob(:,:)
        do i=1,N
                do j=1,N

                        if(pob_aux(i,j)==0) then
                                ! K es numero de enfermos alrededor kmin=0 kmax=4
                                k= pob_aux(fro(i+1),fro(j)) + pob_aux(fro(i-1),fro(j)) + pob_aux(fro(i),fro(j+1)) + pob_aux(fro(i),fro(j-1))  

                                call random_number(x)
                                nivel = 0.2*k

                                if(x < nivel) then 

                                        pob(i,j) = 1         

                                endif


                        endif  

                enddo
        enddo 

        nt= sum(pob)
        dias= dias + 1
        print *, dias, nt

enddo


end program expansion 
