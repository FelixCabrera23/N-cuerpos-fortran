! 2020 - 19 - 5
! N_cuerpos_e.f90
! Félix Cabrera (walberto.cabrera@gmail.com)

! SIMULACIÓN DEL PROBLEMA DE N CUERPOS

! Codificación del texto: ASCII text
! Compiladores probados: GNU Fortran (Ubuntu 9.2.1-9ubuntu2) 9.2.1 2019008

! ARCHIVOS DE CONFIGURACIÓN: 
! potencial.config
! Newton.config
! Coulomb.config
! Requiere: funciones.f90

! Instrucciones de compilación:
! gfortran -Wall -pedantic -std=f95 -c Fuerza.f90
! gfortran -Wall -pedantic -std=f95 -c funciones.f90
! gfortran -Wall -pedantic -std=f95 -c aceleracion.f90
! gfortran -Wall -pedantic -std=f95 -c funciones2.f90
! gfortran -Wall -pedantic -std=f95 -c N_cuerpos_e.f90
! gfortran -Wall -pedantic -std=f95 -o N_cuerpos Fuerza.o funciones.o aceleracion.o funciones2.o N_cuerpos_e.o
! ./N_cuerpos

! Para caracterizar
! /usr/bin/time -f "%e, %M, %P," ./N_cuerpos

PROGRAM n_cuerpos
  USE funciones
  USE funciones2
  
  IMPLICIT NONE
  
  ! Definimos variables del problema
  REAL(8), ALLOCATABLE :: Particula(:,:) ! x, y, masa, carga vs numero de particulas
  INTEGER(4) :: N, pasos, datos  ! Número de particulas
  REAL(8) :: Konstante
  
  ! Variables auxiliares
  REAL(8), ALLOCATABLE :: Acel(:,:),Posisiones(:,:), Velocidades(:,:)
  INTEGER(4) :: i,j,k,l, contador
  LOGICAL :: gravitacion
  
  ! Variables de control
  INTEGER(4) :: err
  
  ! Abrimos el archivo de configuración (12)
  OPEN (12, FILE='potencial.config', STATUS='old', IOSTAT=err)
  IF (err .ne. 0) STOP 'potencial.config is missing'
  
  READ(12,*)
  READ(12,*) gravitacion
  READ(12,*) pasos
  READ(12,*) Konstante
  READ(12,*) datos
  
  CLOSE (12)
  ! Configuramos el programa para funciónar con  el potencial indicado
  ! Esta parte configura el programa y llena la variable Particulas
    IF (gravitacion) THEN
    ! Abrimos el archivo con las condiciones iniciales
    OPEN (14, FILE='Newton.config', STATUS='old', IOSTAT=err)
    IF (err .ne. 0) STOP 'Newton.config is missing'
    READ (14,*) N
    ! Definimos las dimensiones de la variable particulas
    ALLOCATE(Particula(5,N))
    DO i=1, N ! pasamos los datos a la lista
      READ (14,*) Particula(:,i)
    END DO
    CLOSE (14)
  ELSE
    ! Abrimos el archivo con las condiciones iniciales
    OPEN (14, FILE='Coulomb.config', STATUS='old', IOSTAT=err)
    IF (err .ne. 0) STOP 'Newton.config is missing'
    READ (14,*) N 
    ! Definimos las dimensiones de la variable particulas
    ALLOCATE(Particula(6,N))
    DO i=1, N  ! pasamos los datos a la lista
      READ (14,*) Particula(:,i)
    END DO
    CLOSE(14)
    
  END IF
  
  ! Delcaramos variables auxiliares
  ALLOCATE(Acel(2,N),Posisiones(N*2,datos+1),Velocidades(N*2,datos+1))
  !Ponemos las posiciones iniciales
  j=1
  k=2
  DO l=1,N
    Posisiones(j,1) = Particula(1,l)
    Posisiones(k,1) = Particula(2,l)
    Velocidades(j,1)= Particula(3,l)
    Velocidades(k,1)= Particula(4,l)
    j = j + 2
    k = k + 2 
  END DO
  
  ! Abrimos el acrchivo donde escribiremos los resultados
  OPEN (16, FILE='Posiciones.dat', STATUS='new',IOSTAT=err)
  IF (err .ne. 0) STOP 'Posiciones.dat exists already'
  WRITE (16,*) '#Resultados del movimiento del problema de N cuerpos para', N,'particulas'
  OPEN (18, FILE='Velocidades.dat', STATUS='new', IOSTAT=err)
  IF (err .ne. 0) STOP 'Velocidades.dat exists already'
  WRITE (18,*) '#Velocidades del problema de N cuerpos para', N,'particulas'
  ! Escribimos la pocision inicial 

  PRINT *, '***********************************************************************'
  PRINT *, '  Resolviendo problema de fuerza central para ',N,'Cuerpos'
  IF (gravitacion) THEN
    PRINT *, '  Fuerza de atracción Gravitatoria:'
  ELSE
    PRINT *, '  Fuerza de Coulomb:'
  END IF
  PRINT *, '  Constante de proporcionalidad: ', Konstante
  PRINT *, '  Realizando', pasos, 'iteraciones'
  PRINT *, '  Procesando...'


  !XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
  ! Esta parte realiza el movimiento
  contador = 2
  DO i=1, pasos
    Acel = Aceleracion(Particula,N,gravitacion,konstante)
    Particula(1,:) = Particula(1,:) + Particula(3,:)+(0.5)*Acel(1,:)
    Particula(2,:) = Particula(2,:) + Particula(4,:)+(0.5)*Acel(2,:)
    Particula(3,:) = Particula(3,:) + Acel(1,:)
    Particula(4,:) = Particula(4,:) + Acel(2,:)
    IF (MOD(i,(pasos/datos))==0) THEN
      j=1
      k=2
      DO l=1,N
        
        Posisiones(j,contador) = Particula(1,l)
        Posisiones(k,contador) = Particula(2,l)
        Velocidades(j,contador)= Particula(3,l)
        Velocidades(k,contador)= Particula(4,l)
        j = j + 2
        k = k + 2 
        
      END DO
      contador = contador + 1  
    END IF  
    IF (MOD(i,(pasos/20))==0) THEN
      write(*, fmt='(1x,a,i0)', advance='no') '%'
    END IF
  END DO
  
  DO i=1,datos
    WRITE (16,*) Posisiones(:,i)
    WRITE (18,*) Velocidades(:,i)
  END DO
  CLOSE(16)
  CLOSE(18)
  DEALLOCATE(Acel,Posisiones,Particula,Velocidades)
  PRINT *, 'Fin'
  PRINT *, '***********************************************************************'
END PROGRAM n_cuerpos
