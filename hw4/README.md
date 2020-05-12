# hw4

For example, to compile the `pi` source code, I used

```
mpif90 pi.f90 -o pi
```

You can do the same to all other `.f90` files.  

and to execute it, I used the following.

```
mpirun -np 5 hello 
mpirun -np 2 send-recv
mpirun -np 2 ping-pong
mpirun -np 2 latency
mpirun -np 5 ring
mpirun -np 5 pi
```

The standard outputs is also logged into corresponding `.out` files.   
