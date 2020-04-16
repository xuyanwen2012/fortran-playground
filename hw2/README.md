# hw2

To compile the source code, I used

```
gfortran ones.f90 -o ones
```
and

```
gfortran trap.f90 -o trap
```

## trap.f90

### Q1: What are the right answers?

* For f(x) = x^2 from 0 to 2, the right answer is **8/3** or **2.6666...**
* For f(x) = sin(x) from 0 to PI, the right answer is  **2**

### Q2: How many intervals do you have to use to get a decent answer? 

I used `N = 10000` and I think I got a pretty decent answer (see below). It really depends on how many digit of precision you want. 

Example 1 input and output for trap.f90: 

```
-bash-4.2$ ./trap
 Enter lower & upper bound (two reals):
0 2
 Enter interval N (one integer):
10000
 Enter 1 for f(x) = x^2, enter 2 for f(x) = sin(x) (one integer either 1 or 2):
1
 result =    2.66674185
```

Example 2 input and output for trap.f90: 

```
-bash-4.2$ ./trap
 Enter lower & upper bound (two reals):
0 3.1415926535897932
 Enter interval N (one integer):
10000
 Enter 1 for f(x) = x^2, enter 2 for f(x) = sin(x) (one integer either 1 or 2):
2
 result =    1.99997425
```

## ones.f90 

Example 1 input and output for ones.f90: 

```
-bash-4.2$ ones
 Enter the dimension (one integer):
5
 The randomly generated array looks like this:
 0 0 0 0 0
 1 1 1 0 0
 0 0 1 0 0
 0 1 0 0 0
 0 0 0 1 0
 The second array looks like this:
 0 1 0 0 0
 0 1 0 0 0
 1 1 1 0 0
 0 0 1 0 0
 0 0 0 0 0
```
