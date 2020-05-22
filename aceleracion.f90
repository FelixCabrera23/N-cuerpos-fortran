! 2020 - 19 - 5
! aceleracion.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! FORMA PARTE DE N CUERPOS
! Calcula la fuerza entre dos particulas

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: 
! gfortran -Wall -pedantic -std=f95 -c aceleracion.f90 

FUNCTION Aceleracion(Sistema,N,grav,k)
  USE funciones

  IMPLICIT NONE
  
  ! Definimos variable externas
  REAL(8), INTENT(IN) :: k, Sistema(:,:) ! Constante y sistema de particulas
  INTEGER(4), INTENT(IN) :: N !Numero de particulas
  LOGICAL, INTENT(IN) :: grav ! configuracion de potencial
  ! Variables internas
  REAL(8), dimension(2,N) :: Aceleracion
  REAL(8) :: x,y,s,ax,ay, Fn
  INTEGER(4) :: tag, i, j
  REAL(8), ALLOCATABLE :: P1(:), P2(:)
  
  ! Iniciamos las variables
  tag = 6
  IF (grav) tag=tag-1
  ALLOCATE(P1(tag),P2(tag))
  ! Calculamos
  DO i=1, N
    P1 = Sistema(:,i)
    ax = 0
    ay = 0
    DO j=1, N
      IF (i==j) CYCLE
      P2 = Sistema(:,j)
      
      s = SQRT((P1(1)-P2(1))**2+(P1(2)-P2(2))**2)
      x = P2(1)-P1(1)
      y = P2(2)-P1(2)
      Fn = (Fuerza(grav,k,P1,P2))/P1(5)
      ax = ax + Fn*(x/s)
      ay = ay + Fn*(y/s)
    END DO
    Aceleracion(1,i)=ax
    Aceleracion(2,i)=ay
  END DO

END FUNCTION Aceleracion
