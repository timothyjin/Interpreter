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