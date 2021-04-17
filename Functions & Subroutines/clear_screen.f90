!===============================================================================================
! This subroutine can clear the screen in UNIX like systems printing the characters printed by
! the UNIX command 'clear' you can get the characters by "clear > temp"
! The statement is equivalent to -   print *, "[H[2J[3J"
! (ACHAR(27) returns ASCII 27)
! Shriraj Hegde 2020
!===============================================================================================

subroutine clearScreen
    print *,achar(27),"[H",achar(27),"[2J",achar(27),"[3J" 
end subroutine clearScreen
