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
