! 2020 - 19 - 5
! funciones.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! FORMA PARTE DE N CUERPOS
! MODULO DE INTERFASE DE FUNCIÓNES

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008
! Instrucciones de compilación: requiere Fuerza.f90, aceleracion.f90
! gfortran -Wall -pedantic -std=f95 -c aceleracion.f90
! gfortran -Wall -pedantic -std=f95 -c Fuerza.f90
! gfortran -Wall -pedantic -std=f95 -c funciones.f90

MODULE funciones2

  ! Aceleración
  INTERFACE
    FUNCTION Aceleracion(Sistema,N,grav,k)
        REAL(8), INTENT(IN) :: k, Sistema(:,:)
        INTEGER(4), INTENT(IN) :: N
        LOGICAL, INTENT(IN) :: grav
        REAL(8), dimension(2,N) :: Aceleracion
    END FUNCTION
  END INTERFACE

END module funciones2
