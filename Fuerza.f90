! 2020 - 19 - 5
! fuerza.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! FORMA PARTE DE N CUERPOS
! Calcula la fuerza entre dos particulas

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: 
! gfortran -Wall -pedantic -std=f95 -c -o Fuerza.o Fuerza.f90 

FUNCTION Fuerza(grav,k,P1, P2)
  IMPLICIT NONE
  
  ! Definimos variables externas
  LOGICAL, INTENT(IN) :: grav
  REAL(8), INTENT(IN) :: P1(:), P2(:), k
  ! Variables internas
  REAL(8) :: r2, Fuerza
  INTEGER(4) :: tag
  
  tag = 6 ! Cambiara la forma de esta funcion dependiendo de la configuración
  IF (grav) tag=tag-1 ! Esto cambia dependidiendo de la configuración
  ! Calculo de la distancia
  r2 = (P1(1)-P2(1))**2+(P1(2)-P2(2))**2
  Fuerza =((-1)**(tag-1))*k*(P1(tag)*P2(tag))/r2
  RETURN

END FUNCTION Fuerza

