!============================================================================
! Prints 2D array as a matrix without requiring any other parameter than array to be printed
! Edit to get required formatting
! Shriraj Hegde - 2020
!============================================================================


subroutine printMatrix(array_2d)
        implicit none
        integer::i,j,n,m,t(2)
        real*8,intent(IN)::array_2d(:,:)
        t=SHAPE(array_2d);n=t(1);m=t(2) !determining shape
        do j=1,n
            do i=1,m
                write(*,"(F6.2)",advance='no') array_2d(j,i)  !add formatting as required
                write(*,"(A2)",advance='no')" "
            enddo
            do i=1,2   !(1,k) where k is the number of empty rows inbetween rows
                print*,''
            enddo 
        enddo
    end subroutine printMatrix
