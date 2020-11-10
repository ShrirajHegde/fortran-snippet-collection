# Fortran modules and snippets collection
Collection of modules with subroutines and functions for fortan from various places,aim is to make subroutines way simpler than ones in LAPACK.


**Feel free to use,contribute,create issues and pull requests.**

#### Using the following line in fortran to use the module from another file or directly copy paste the whole module before the main program
```f90
INCLUDE 'file.ext'
```
#### Add the following line to use the module in the program

```f90
program main
  use 'module_name'
  implicit none
  !(program)
end program main
```
