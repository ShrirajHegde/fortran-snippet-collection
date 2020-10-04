***
### In fortran ACHAR(N) returns the ASCII of N

    WRITE(*,'(2f15.9,A5)',advance='no') float1,float2,ACHAR(13)

ACHAR(13) returns carriage return character **"\r"** in python. So after printing, it returns the cursor to the beginning of the line where it can be overwritten.

After you are out of the loop you can use `CALL SYSTEM('clear')` to clean the screen. 

### This is helpful since `CALL SYSTEM('clear')` is slower and uses a lot of CPU,you can check this by replacing the above method with <br>

    WRITE(*,'(2f15.9)',advance='no') float1,float2
    CALL SYSTEM('clear')

and check the difference in time taken in loops.
***
