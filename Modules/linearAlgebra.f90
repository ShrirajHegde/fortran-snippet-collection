
!============================================================
!Procedure Collection by Shriraj Hegde
!-
!References:
!https://ww2.odu.edu/~agodunov/computing/programs/book2/Ch06/Inverse.f90
!https://stackoverflow.com/questions/13293964/module-calling-an-external-procedure-with-implicit-interface
!===========================================================


module linearAlgebra  !!add "use linearAlgebra" in the program,(add the module outside program block)
    implicit none  !!IMPORTANT, ALWAYS PLACE MODULES BEFORE PROGRAM else you will face (.mod) missing error
    contains
    subroutine printMatrix(array_2d)
        implicit none
        integer::iterator1pMsub,iterator2Pmsub,shapeHolderpmsub,shapeholder2pmsub,t(2)
        real*8,intent(IN)::array_2d(:,:)
        t=SHAPE(array_2d);shapeHolderpmsub=t(1);shapeholder2pmsub=t(2) !determining shape
        do iterator2Pmsub=1,shapeHolderpmsub 
            do iterator1pMsub=1,shapeholder2pmsub
                write(*,"(F6.2)",advance='no') array_2d(iterator2Pmsub,iterator1pMsub)  !add formatting as required
                write(*,"(A2)",advance='no')" "
            enddo
            do iterator1pMsub=1,2   !(1,k) where k is the number of empty rows inbetween rows
                print*,''
            enddo 
        enddo
    end subroutine printMatrix

    subroutine printVector(vector_1d_array)
        implicit none
        integer::iterator1pvsub,sizeholderpvsub,iterator2pvsub
        real*8,intent(IN)::vector_1d_array(:)
        sizeholderpvsub=size(vector_1d_array)
        do iterator1pvsub=1,sizeholderpvsub
                write(*,"(F14.8)",advance='no')vector_1d_array(iterator1pvsub)
            do iterator2pvsub=1,1   !(1,k) where k is the number of empty rows inbetween rows
                print*,''
            enddo 
        enddo
        print*,''
    end subroutine printVector

    function readMatrix(number_of_rows,number_of_columns)
        implicit none
        INTEGER,INTENT(IN)::number_of_rows,number_of_columns
        integer::mreadMatrix
        REAL*8::readMatrix(number_of_rows,number_of_columns)
        do mreadMatrix=1,number_of_rows
            READ*,readMatrix(mreadMatrix,:)
        enddo
    end function readMatrix

    function inverse(square_matrix)
        !============================================================
        ! Inverse matrix
        ! Method: Based on Doolittle LU factorization for Ax=b
        ! Alex G. December 2009
        ! Modified Shriraj Hegde - September 2020 (preserves input matrix,modified to function)
        !-----------------------------------------------------------
        ! input ...
        ! square_matrix(dimension,dimension) - array of coefficients for matrix A
        ! dimension      - dimension of square matrix
        ! output ...
        ! inverse(dimension,dimension) - inverse matrix of A
        ! get dimension of matrix by shape(matrix) which returns a 1d array
        !===========================================================
        implicit none 
        integer dimension
        DOUBLE PRECISION,INTENT(IN):: square_matrix(:,:) !preserving the original matrix
        double precision,allocatable ::a(:,:), inverse(:,:)
        double precision,allocatable:: L(:,:), U(:,:), b(:), d(:), x(:)
        double precision coeff
        integer i, j, k,t(2)

        t=shape(square_matrix)

        if (t(1) .ne. t(2) ) then
            print*,"ERROR : NOT A SQUARE MATRIX (inverse function)"
            call exit
        endif

        dimension=t(1)


        ALLOCATE(a(dimension,dimension))
        ALLOCATE(inverse(dimension,dimension))
        ALLOCATE(L(dimension,dimension))
        ALLOCATE(U(dimension,dimension))
        ALLOCATE(b(dimension))
        ALLOCATE(d(dimension))
        ALLOCATE(x(dimension))

        
        
        !for preserving input matrix
        a=square_matrix;
        ! step 0: initialization for matrices L and U and b
        ! Fortran 90/95 aloows such operations on matrices
        L=0.0
        U=0.0
        b=0.0

        ! step 1: forward elimination
        do k=1, dimension-1
        do i=k+1,dimension
            coeff=a(i,k)/a(k,k)
            L(i,k) = coeff
            do j=k+1,dimension
                a(i,j) = a(i,j)-coeff*a(k,j)
            end do
        end do
        end do

        ! Step 2: prepare L and U matrices 
        ! L matrix is a matrix of the elimination coefficient
        ! + the diagonal elements are 1.0
        do i=1,dimension
        L(i,i) = 1.0
        end do
        ! U matrix is the upper triangular part of A
        do j=1,dimension
        do i=1,j
            U(i,j) = a(i,j)
        end do
        end do

        ! Step 3: compute columns of the inverse matrix C
        do k=1,dimension
        b(k)=1.0
        d(1) = b(1)
        ! Step 3a: Solve Ld=b using the forward substitution
        do i=2,dimension
            d(i)=b(i)
            do j=1,i-1
            d(i) = d(i) - L(i,j)*d(j)
            end do
        end do
        ! Step 3b: Solve Ux=d using the back substitution
        x(dimension)=d(dimension)/U(dimension,dimension)
        do i = dimension-1,1,-1
            x(i) = d(i)
            do j=dimension,i+1,-1
            x(i)=x(i)-U(i,j)*x(j)
            end do
            x(i) = x(i)/u(i,i)
        end do
        ! Step 3c: fill the solutions x(n) into column k of C
        do i=1,dimension
            inverse(i,k) = x(i)
        end do
        b(k)=0.0
        end do

        inverse=inverse;
    end function inverse



    REAL*8 FUNCTION determinant(square_matrix)
        IMPLICIT NONE
        REAL*8, DIMENSION(:,:),INTENT(IN) :: square_matrix!preserving input
        REAL*8, DIMENSION(:,:),allocatable :: matrix
        REAL*8 :: m, temp
        INTEGER :: i, j, k, l,dimension,t(2)
        LOGICAL :: DetExists = .TRUE.
        t = shape(square_matrix)
        if (t(1) .ne. t(2)) then
            print*,"ERROR : NOT A SQUARE MATRIX (determinant function)"
            call exit
        endif

        allocate(matrix(t(1),t(1)))

        l = 1
        matrix = square_matrix
        !Convert to upper triangular form
        DO k = 1, dimension-1
            IF (matrix(k,k) == 0) THEN
                DetExists = .FALSE.
                DO i = k+1, dimension
                    IF (matrix(i,k) /= 0) THEN
                        DO j = 1, dimension
                            temp = matrix(i,j)
                            matrix(i,j)= matrix(k,j)
                            matrix(k,j) = temp
                        END DO
                        DetExists = .TRUE.
                        l=-l
                        EXIT
                    ENDIF
                END DO
                IF (DetExists .EQV. .FALSE.) THEN
                    determinant = 0
                    return
                END IF
            ENDIF
            DO j = k+1, dimension
                m = matrix(j,k)/matrix(k,k)
                DO i = k+1, dimension
                    matrix(j,i) = matrix(j,i) - m*matrix(k,i)
                END DO
            END DO
        END DO
    
        !Calculate determinant by finding product of diagonal elements
        determinant = l
        DO i = 1, dimension
            determinant = determinant * matrix(i,i)
        END DO
    
    END FUNCTION determinant

    subroutine swapVector(vector1,vector2)
        implicit none
        real*8,INTENT(INOUT)::vector1(:),vector2(:)
        real*8,ALLOCATABLE::temp_vec(:)
        INTEGER::dimensioswp
        if(size(vector1) .ne. size(vector2))then
            print*,"Error,vectors/slices are of different dimension (swapVector subroutine)"
            CALL exit
        endif
        dimensioswp = size (vector1)
        ALLOCATE(temp_vec(dimensioswp))
        temp_vec = vector2; vector2 = vector1 ;vector1 = temp_vec
    end subroutine swapVector
    
    subroutine swapReal(real1,real2)
        implicit none
        real*8,INTENT(INOUT)::real1,real2
        real*8::tempRea=0
        tempRea=real1; real1=real2; real2=tempRea
    end subroutine swapReal

    subroutine swapInt(int1,int2)
        implicit none
        INTEGER,INTENT(INOUT)::int1,int2
        INTEGER::tempInt=0
        tempInt=int1; int1=int2; int2=tempInt
    end subroutine swapInt


end module


