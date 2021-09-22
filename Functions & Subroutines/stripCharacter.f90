!taken from 
!https://stackoverflow.com/questions/27179549/removing-whitespace-in-string


RECURSIVE FUNCTION stripper(string,ch) RESULT(stripped)
    CHARACTER(len=*), INTENT(in) :: string
    CHARACTER, INTENT(in) :: ch
    CHARACTER(:), ALLOCATABLE :: stripped

    IF (LEN(string)==1) THEN
        IF (string==ch) THEN 
            stripped = ''
        ELSE
            stripped = string
        END IF
    ELSE
        IF (string(1:1)==ch) THEN
            stripped = stripper(string(2:),ch)
        ELSE
            stripped = string(1:1)//stripper(string(2:),ch)
        END IF
    END IF
END FUNCTION stripper
