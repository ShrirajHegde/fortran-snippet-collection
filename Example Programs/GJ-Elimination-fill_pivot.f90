!==============================================================================================
! Shriraj Hegde - Oct 2020 
!==============================================================================================

program main
    implicit none
    REAL*8, dimension (:,:), allocatable :: augmented_matrix
    REAL*8, dimension (:), allocatable ::temp_row,solution
    Integer,allocatable::solution_matrix_order(:)
    INTEGER::dim,maxlocation(2),i,j
    
    print*,'__________________________________________________________'
    print*,'Enter number of Equations(Integer):'
    read*,dim !taking input of number of dimensions

    !allocating all dynamic arrays
    ALLOCATE(temp_row(dim+1))
    ALLOCATE(augmented_matrix(dim,dim+1))
    ALLOCATE(solution(dim))
    ALLOCATE(solution_matrix_order(dim));do i=1,dim;solution_matrix_order(i)=i;end do !initialising a matrix to keep track of solution matrix (1,2,3......)

    !taking input of augmented mattix
    print*,'__________________________________________________________'
    print*,"Enter augmented matrix(Coefficient matrix with constant matrix appended as last column):"
    augmented_matrix = readMatrix(dim,dim+1) !returns input matrix
    print*,'__________________________________________________________'
    print*,"Entered augmented matrix is :",achar(10) !achar(10) is newline character
    CALL printMatrix(augmented_matrix) !prints array in matrix form
    
    

    !using gauss-jordan elimination ===============================================================================================================
    do i=1,dim
            CALL full_pivot(augmented_matrix(i:dim,i:dim),augmented_matrix,solution_matrix_order) !calling pivot subroutine before each row
        do j=1,dim
            if (i .ne. j) then
!               Print*,"|------before operation-------",i;CALL printMatrix(augmented_matrix);
                augmented_matrix(j,:) = augmented_matrix(j,:) - (augmented_matrix(j,i)/augmented_matrix(i,i))*augmented_matrix(i,:)
!               Print*,"|------after operation-------",i;CALL printMatrix(augmented_matrix);
            end if
        enddo
    enddo

    !======================================================================================
    !calculating solution
    !======================================================================================
    do i=1,dim
        solution(solution_matrix_order(i))=augmented_matrix(i,dim+1)/augmented_matrix(i,i) !calculating solution in order
    enddo

    !printing obtained diagonal matrix(not in right order)[OPTIONAL]
        ! print*,'__________________________________________________________'
        ! print*,'Diagonal Matrix obtained by Gauss-Jordan Elimination (Not in order) :',ACHAR(10) !achar returns newline character {which is ASCII 10}
        ! CALL printMatrix(augmented_matrix)
        

    !Calculating and printing obtained solution(solution is in solution array)
    print*,'__________________________________________________________'
    print*,'Solution of the system of equations is:',achar(10)
    do i=1,dim
        WRITE(*,'(A3,I1,f15.9)')'  x',i,solution(i) !change 'I1' to I2 if dim>9 and to I2 if dim >99 and so on...
    enddo
    print*,'__________________________________________________________'
    !=====================================================================================================================================
    
    contains 

    !PROCEDURE FOR FULL PIVOT

    subroutine full_pivot(sub_matrix,full_matrix,order_matrix)
        implicit none
        REAL*8,INTENT(INOUT)::sub_matrix(:,:),full_matrix(:,:)
        integer::order_matrix(:)
        maxlocation=MAXLOC(ABS(sub_matrix)) !maxloc returns location of max element WRT submatrix (which needs to be adapted)
!        print*,'supplied:';call printMatrix(sub_matrix)
        if(size(sub_matrix)==1)then
            return
        endif
        if (maxlocation(1)==1 .and. maxlocation(2)==1)then !if the matrix is already in required state
            RETURN!return without any modification
        else if(maxlocation(2)==1)then!if largest element is in 1st column
            call swapVector(full_matrix(maxlocation(1)+i-1,:),full_matrix(i,:)) !swapping row with largest element with first row(in the full matrix) ALONG WITH CHANGING MAXLOCATION TO INDEX OF ORIGINAL MATRIX
        else if (maxlocation(2).ne.1)then !if largest element is not in first column
            CALL swapVector(full_matrix(:,maxlocation(2)+i-1),full_matrix(:,i)) !swap element with largest column with first column(in the full matrix) ALONG WITH CHANGING MAXLOCATION TO INDEX OF ORIGINAL MATRIX
            CALL swapInt(order_matrix(maxlocation(2)+i-1),order_matrix(i))!Keeping track of order of columns
            maxlocation=MAXLOC(ABS(sub_matrix)) !update location of maximum element in updated sub-matrix
                if(maxlocation(2)==1)then!if largest element is in 1st column
                call swapVector(full_matrix(maxlocation(1)+i-1,:),full_matrix(i,:)) !swapping row with largest element with first row(in the full matrix) ALONG WITH CHANGING MAXLOCATION TO INDEX OF ORIGINAL MATRIX
                endif
        endif
!        print*,'returned:';call printMatrix(sub_matrix)
    end subroutine full_pivot

!==============================================================================================================================================
! Nonspecific Procedures
!==============================================================================================================================================

    function readMatrix(number_of_rows,number_of_columns)
        implicit none
        INTEGER,INTENT(IN)::number_of_rows,number_of_columns
        integer::mreadMatrix
        REAL*8::readMatrix(number_of_rows,number_of_columns)
        do mreadMatrix=1,number_of_rows
            READ*,readMatrix(mreadMatrix,:)
        enddo
    end function readMatrix


    subroutine printMatrix(array_2d)
        implicit none
        integer::i11111,j11111,n11111,m11111,t(2)
        real*8,intent(IN)::array_2d(:,:)
        t=SHAPE(array_2d);n11111=t(1);m11111=t(2) !determining shape
        do j11111=1,n11111 
            do i11111=1,m11111
                write(*,"(F6.2)",advance='no') array_2d(j11111,i11111)  !add formatting as required
                write(*,"(A2)",advance='no')" "
            enddo
            do i11111=1,2   !(1,k) where k is the number of empty rows inbetween rows
                print*,''
            enddo 
        enddo
    end subroutine printMatrix

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

    subroutine swapInt(int1,int2)
        implicit none
        INTEGER,INTENT(INOUT)::int1,int2
        INTEGER::tempInt=0
        tempInt=int1; int1=int2; int2=tempInt
    end subroutine swapInt
!==============================================================================================================================================

end program main