### Contributed by **Muhseen Mustafa**
---
If you want to write a format statement but you have use a variable (so you can't put it inside the format string) use write to make a character for example,

#### say you want following format for:
    WRITE (*,"(3f10.3) ")  MATRIX

#### to generalise  it for n instead of you can use a statement like:
    WRITE (string,"(A,I1,A,I1,A)") "(",n,"F10.3,A5,F10.3,A5,",n,"F10.3)"

#### which will set string = **(3F10.3,A5,F10.3,A5,3F10.3)**

#### then use the string as format in write statements like:
    WRITE(*,string)

