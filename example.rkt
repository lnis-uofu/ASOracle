#lang linear_diff
# Setup
eia-96
# design thing
vdd = 9
vss = -9
gnd = 0

# IO
input y
input x
output w
output z
output v
# Expressions
a = 2
v = x + 2
b = y + a
c = 1 + 2 * 5
z = integrate(-4 * x + 2)
w = 5 * b + - z + c
