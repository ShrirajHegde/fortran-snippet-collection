!-----------------------------------------------------------------
        SUBROUTINE DATIME
!
!       This subroutine returns the DATE AND TIME
!
      Character*24 datentime 
      CALL FDATE (DateNTime) 
      Return
      END                 
!-----------------------------------------------------------------
    SUBROUTINE CONCATENATE_FIVE(strOut,intLenOut,str01,str02,str03,str04,str05)
    USE VICA_VARS
    IMPLICIT NONE
    INTEGER(4) intLenIn,intLenOut
    CHARACTER(*) strOut,str01,str02,str03,str04,str05

    intLenOut=0
    CALL FindLen(str01,intLenIn)
    strOut=str01(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str02,intLenIn)
    strOut=strOut(:intLenOut) // str02(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str03,intLenIn)
    strOut=strOut(:intLenOut) // str03(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str04,intLenIn)
    strOut=strOut(:intLenOut) // str04(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str05,intLenIn)
    strOut=strOut(:intLenOut) // str05(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !       
    CALL REPLACEWITHSPACE(strOut,"b",1,' ',1)      
    CALL FindLen(strOut,intLenOut)

    END        
!*****************************************************************
    SUBROUTINE CONCATENATE10(strOut,intLenOut,str01,str02,str03,str04,str05,str06,str07,str08,str09,str10)
    USE VICA_VARS
    IMPLICIT NONE
    INTEGER(4) intLenIn,intLenOut
    CHARACTER(*) strOut,str01,str02,str03,str04,str05,str06,str07,str08,str09,str10

    intLenOut=0
    CALL FindLen(str01,intLenIn)
    strOut=str01(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str02,intLenIn)
    strOut=strOut(:intLenOut) // str02(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str03,intLenIn)
    strOut=strOut(:intLenOut) // str03(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str04,intLenIn)
    strOut=strOut(:intLenOut) // str04(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str05,intLenIn)
    strOut=strOut(:intLenOut) // str05(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str06,intLenIn)
    strOut=strOut(:intLenOut) // str06(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str07,intLenIn)
    strOut=strOut(:intLenOut) // str07(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str08,intLenIn)
    strOut=strOut(:intLenOut) // str08(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str09,intLenIn)
    strOut=strOut(:intLenOut) // str09(:intLenIn)
    intLenOut=intLenOut+intLenIn
    !
    CALL FindLen(str10,intLenIn)
    strOut=strOut(:intLenOut) // str10(:intLenIn)
    intLenOut=intLenOut+intLenIn
        
    CALL REPLACEWITHSPACE(strOut,"b",1,' ',1)      
    CALL FindLen(strOut,intLenOut)

END        
!-----------------------------------------------------------------
        SUBROUTINE FindLen(dstr,nllen)
!
!       This subroutine finds length of a string (up to trailing blanks)
!
        Character*(*) dstr
        INTEGER*4 ls,II,nllen  !,intL
        
        ls=len(dstr)                  !stash string length
        do II=ls,1,-1                  !for each character...
          IF(dstr(II:II).gt.' ' .or. dstr(II:II).eq.char(12) .OR. dstr(II:II).eq.char(6)) THEN
                                      !find nonblank or control 
                                      !character or a form feed?
            nllen=II                   !no-stash length
            return                    !Au revoir
          ENDIF
        ENDDO
        nllen=0                       !provide for null string?
        return
    end
!========================================================
    SUBROUTINE COUNT_LEADING_SPACES(str2Scan,intLeadingSpaces) 

    USE VICA_VARS 
    IMPLICIT NONE
    INTEGER(4) ii,intLeadingSpaces,intLimit
    CHARACTER(*) str2Scan
    
    CALL FINDLEN(str2Scan,intLimit) 
    intLeadingSpaces=0
    IF(intLimit.EQ.0) THEN
        RETURN
    ENDIF
    DO ii=1,intLimit
        IF(str2Scan(ii:ii).NE.' ') THEN
            intLeadingSpaces=ii-1
            RETURN
        ENDIF
    END DO

    END
!========================================================
    SUBROUTINE COUNT_CHARS(str2Check,str1char,intQtyFound)

    USE VICA_VARS 
    IMPLICIT NONE
    LOGICAL(4) blnFound
    INTEGER(4) intQtyFound,intPos
    CHARACTER(*) str2Check,str1Char
    CHARACTER(200) strWork
    
    intQtyFound=0
    strWork=str2Check
    blnFound=.TRUE.
    DO WHILE(blnFound.EQ. .TRUE.)
        intPos=INDEX(strWork,str1char)
        IF(intPos.GT.0) THEN
            strWork=strWork(intPos+1:)
            intQtyFound=intQtyFound+1
        ELSE
            blnFound=.FALSE.
        ENDIF
    ENDDO
    END
