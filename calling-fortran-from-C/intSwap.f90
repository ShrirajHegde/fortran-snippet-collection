integer function intSwap(int1,int2) bind ( C, name="intSwap_" )
        use iso_c_binding
        implicit none
        type(c_ptr) ::int1,int2,tmpint !special type provided by iso_c_binding
        tmpint=int2;int2=int1;int1=tmpint !swap locations
        intSwap=0 !return exit code
end function intSwap