#include<stdio.h>
#include <stdbool.h> //for using boolean true

extern int intSwap_(int* a, int* b); //declare external function (warning if not defined)

int main ( void ) {
   int a=1;
   int b=0;
   int z; //like an exit code

   printf("a and b variables before calling fortran function - %d %d \n", a, b);
   z=intSwap_(&a,&b); //passing pointers
   if(!z==true){
      printf("a and b variables after  calling fortran function - %d %d  \n", a, b);
   }else{
      printf("Some error occured \n");
   }
   
   return 0;

}
