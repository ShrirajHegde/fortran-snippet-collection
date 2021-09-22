!===========================================================
! Counts and returns number of lines in a text file
!===========================================================

INTEGER function lineCount(filename1_of_50_char_len)
    CHARACTER(len = 50)::filename1_of_50_char_len
    INTEGER::io
    lineCount = 0 
    OPEN (1536, file = filename1_of_50_char_len,status='old')
    DO
        READ(1536,*,iostat=io)
        IF (io.ne.0) EXIT
        lineCount = lineCount + 1
    END DO
    CLOSE (1)
end function lineCount