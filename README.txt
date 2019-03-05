Change from branch 'tcj/part1' (02/17/2019):

1. In 'declare', change the structure so the semantics is more clear
	-The last case the value needs to be parsed into 'M-value' function
2. under 'if-else', change the name of abstraction to "stament" and "else-stament"
3. In 'return' change "M-name" to "M-value"
4. If-else statement change of abstraction, error message deleted
(error will never be reached)
5.Change the name "interpreter" to "interpret"
6.While-interpret added
7.For 'M-value', add a case to address negative sign, a helper method "isRightOperandNull" added.
8.In 'program-interpret', add a condition for return.


Things to do:
1. Should not use "LET"
2. Should return true/false instead of #t/#f
3. Better abstraction for add and remove?


Changes from branch Kai
Abstraction:
Changed (define var-name cdr) to (define var-name cadr)
Changed var-name-c to isDeclared to differentiate from (define var-name cadr)
Added (define get-vars-list car) and (define get-val-list cadr) for stat
- couple other changes to abstraction
fixed return of true/false
removed let
Worked on doing the extra challenge part, got everything to work but test 25. 

^Can't seem to get the right x value without affecting other tests for this.
Have a problem with too many (M-state (___ stmt) state) terms. This is mainly due to retrieving the state upon side effects. 

---------------------------------------------------------------------------------------------------

03/03/2019
Starting interpreter 2.
Polish up the previous code, delete multiple "assign" statement. Modification in M-value methods. All side effects accomplished. Ready for interpreter 2.


03/05/2019
Use call/cc instead of cps. (Do not know how to)
Return added,
break added,
continue added.
Test 1-10 passed.
However, break and continue has an error problem, also when continues and breaks the state does not pop top layer.

