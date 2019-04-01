03/30 Commit:
Box implemented
M-state-outerlayer added
make-closure added
function def and function call in M-state added

Nothing being tested yet

03/31 Commit (tcj/part3):
Added function and funcall interpretation functions.
Have not considered how get the program to run, but all of the functionality should be there.

The program can now run, but there are problems with calling functions other than main.

The function and funcall interpretation methods should work.
The main issue is that when funcall is called on main, it will return the value of the first funcall made in main.
So somewhere a return continuation is being passed in where it should not be passed.

4/1 Commmit (axs/part3)

So I tried really really hard to fix this but just could not. I did get some more cases working. Main issue was that
we were passing in return into the funcall and we should not. We also needed a funcall for value as well as for state.
Here is the summary of test cases right now:

1 Y
2 Y
3 Y
4 Y
5 N (contract violation)
6 N (wrong value)
7 Y (but #t instead of true)
8 N (variable not found)
9 Y
10 N (contract violation)
11 N (variable not found)
12 Y
13 Y
14 N (contract violation)
15 N (contract violation)
16 N (variable not found)
17 Y
18 N (contract violation)
19 N (contract violation)
20 N (variable not found)
