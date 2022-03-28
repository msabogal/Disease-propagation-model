!Codigo de propagacion de enfermedad por 
!Miguel Sabogal Garcia 2021
program expansion2
!
implicit none
!
integer, parameter :: N = 200 ! la raiz cuadrada del numero total de individuos
integer, parameter :: M = 200 ! numero de dias
!
integer :: i,j,k,nt,ni,l,k2,nm
!
real :: x,nivel
!
integer, dimension(N,N) :: pob, pob_aux ! poblacion
integer, dimension(N,N) :: inm, inm_aux ! inmunes
integer, dimension(N,N) :: mut, mut_aux ! muertos 
integer, dimension(N,N) :: dias ! dias de enfermedad 
!
integer, dimension(0:N+1) :: fro ! frontera
!
open(1,file='expansion.dat')
open(2,file='enfermos.dat')
open(3,file='inmunes.dat')
open(4,file='muertos.dat')
!
x = 0.
!
pob(:,:) = 0
inm(:,:) = 0
mut(:,:) = 0
dias(:,:)= 0
!
i = N/2
!
pob(i,i) = 1
!
! vamos a definir un elemento que ayuda con las condiciones de frontera periodicas
!
fro(0) = N
do i = 1,N
 fro(i) = i 
enddo
fro(N+1) = 1
!
!
call random_seed
!
!
do l = 1,M
!
  pob_aux(:,:) = pob(:,:)
  inm_aux(:,:) = inm(:,:)
  mut_aux(:,:) = mut(:,:)
!
  do i = 1,N
    do j = 1,N
!
      if((pob_aux(i,j)==0).and.(inm_aux(i,j)==0).and.(mut_aux(i,j)==0)) then
!  
!  
        k = pob_aux(fro(i+1),fro(j)) + pob_aux(fro(i-1),fro(j)) + pob_aux(fro(i),fro(j+1))  + pob_aux(fro(i),fro(j-1))  
!       k es el numero de primeros vecinos enfermos
!
        k2 = pob_aux(fro(i+1),fro(j+1)) + pob_aux(fro(i-1),fro(j-1)) + pob_aux(fro(i-1),fro(j+1)) + pob_aux(fro(i+1),fro(j-1))   
!       k2 es el numero de segundos vecinos enfermos 
!
        call random_number(x)
!  
        nivel = 0.75*k + 0.4*k2
!  
        if(x<nivel) then
!
          call random_number(x)
!
          if(x<0.33) then   
! 
            inm(i,j) = 1
!
          else         
!
            pob(i,j) = 1
!            
            dias(i,j)= 1 ! se cuenta desde el mismo que se enferma la persona como dia 1 de enfermedad 
!            
          endif
!  
        endif
!
      else 
!              
           if ((dias(i,j)<5).and.(pob(i,j)==1))  then   
!
                dias(i,j)= dias(i,j) + 1 ! aumenta un dia que lleva enfermo
!                
           endif
!
           if ((dias(i,j)==5).and.(inm_aux(i,j)==0).and.(mut_aux(i,j)==0)) then
!
                   pob(i,j)= 0  !Se eliminia de la grilla no contribuye mas a la propagación de la enfermedad
!
                   call random_number(x)
!
                   if (x < 0.9) then
!                           
                     inm(i,j)=1
!                     
                   else
!                           
                     mut(i,j)=1 ! muere el enfermo
!                     
                   endif
!                   
           endif         
!
      endif 
!
    enddo
!    
  enddo
!
  nt = sum(pob) ! Numero de enfermos vivos
  ni = sum(inm) ! Numero de inmunes  
  nm = sum(mut) ! Numero de muertos
!
  write(1,*) l,nt,ni,nm
  print *, l,nt,ni,nm
enddo
!
do i = 1,N
  do j = 1,N
!
    write(2,999) i,j,pob(i,j)
    write(3,999) i,j,inm(i,j)
    write(4,999) i,j,mut(i,j)
!
  enddo
!
  write(2,*)
  write(3,*)
  write(4,*)
!
enddo
!
close(1)
close(2)
close(3)
close(4)
!
999 format(3(I8,1X))
!
end program expansion2
