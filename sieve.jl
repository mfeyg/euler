sieve(N) = IntSet(find(pointer_to_array(ccall(("sieve","sieve.so"),Ptr{Cint},(Cint,),N), N, true) .== 0))
