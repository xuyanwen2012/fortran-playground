# hw6

## Compile

For example, to compile the `hello` source code, I used

```
gfortran hello.f90 -o hello -fopenmp
```

You can do the same to all other `.f90` files.  

## Execution 

I ran my code on `Grape`, and to execute it, I used the following.

```
qsub hello.cmd
qsub mat_v1.cmd
qsub mat_v2.cmd
```

The standard outputs is also logged into corresponding `.o` files.

## Analysis

I did my test on `2000x2000` matrix multiplication. Both test on `4` threads per node. The result was surprising.  

### Version 1 - PARALLEL DO
    
The average time it takes is `~12s`. One more interesting observation is that if I swap the outer loop with the inner loop (i.e. Instead of iterate *i* before *j*, I iterate *j* before *i*) then the program will takes about `~2s` longer to compute. I guess the reason is because of it takes the advantage of cache. However, when running this version of the code without OpenMP, it takes significantly longer to complete. 

### Version 2 - MATMUL WORKSHARE

The average time it takes is `~4s`. Very surprising, this was done much faster than the **PARALLEL DO** version. 


Thus I assume **MATMUL WORKSHARE** works best for question 2.
