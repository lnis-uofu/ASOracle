#lang brag
linear : (design | eia | vdd | vss | gnd | input | output | assign)+
design : /"design" IDENTIFIER
eia : EIA
vdd : /"vdd" /"=" "-"? NUMBER
vss : /"vss" /"=" "-"? NUMBER
gnd : /"gnd" /"=" "-"? NUMBER
input : /"input" IDENTIFIER (/"," IDENTIFIER)*
output : /"output" IDENTIFIER (/"," IDENTIFIER)*
assign : IDENTIFIER /"=" expression

/expression : integration | parenthesis | negation | multiplication | addition | subtraction | variable | constant

variable : IDENTIFIER
constant : NUMBER

;; need to verify the associativity and operator precedence
integration : /"integrate" /"(" expression /")" ;; /"[" IDENTIFIER /"]" implicit delta on time
/parenthesis : /"(" expression /")"
negation : /"-" expression
multiplication : expression /"*" expression
addition : expression /"+" expression
subtraction : expression /"-" expression
division : expression /"/" expression
