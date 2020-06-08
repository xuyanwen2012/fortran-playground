# proj

## Compile

You must build your own executeble if you modified the source code. 
For example, to compile the 1D version GoF `life.f90` source code, I used

```
mpif90 -o life.exe life.f90
```

and for 2D version, do the same

```
mpif90 -o life2d.exe life2d.f90
```

## Execution 

I ran my code on `Grape`, and to execute it, I used the following.

```
qsub life.cmd
qsub life2d.cmd
```

or alternately, you can do

```
mpirun -np 4 life.exe
```

The standard outputs is also logged into corresponding `.o` files. Also the board state at each step was logged into the `gof_0XX.dat` file, the `XX` is subsituted with the step like `gof_019.dat`. 

## Analysis

Please refer to the `.pdf` file for the report on the performance analysis. 

