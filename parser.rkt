#lang brag
/linear-diff : (design | eia | vdd | vss | gnd | input | output | assign)+

design : /"design" IDENTIFIER
eia : EIA
vdd : /"vdd" /"=" "-"? NUMBER
vss : /"vss" /"=" "-"? NUMBER
gnd : /"gnd" /"=" "-"? NUMBER
input : /"input" IDENTIFIER (/"," IDENTIFIER)*
output : /"output" IDENTIFIER (/"," IDENTIFIER)*
assign : IDENTIFIER /"=" expression

@expression : integrate | parenthesis | negation | multiplication | addition | subtraction | net | constant

net : IDENTIFIER
constant : NUMBER

integrate : /"integrate" /"(" expression /")" ;; /"[" IDENTIFIER /"]" implicit delta on time

/parenthesis : /"(" expression /")"
negation : /"-" expression
multiplication : expression /"*" expression
;; need to verify the associativity and operator precedence
addition : expression /"+" expression
subtraction : expression /"-" expression
