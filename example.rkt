#lang linear_diff
# Setup
eia-96
design thing
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
f = 1
a = 2
v = a
b = y + a
c = 1 + 2 * 5
z = integrate(-4.5 * x + 2)
w = 5 * y + - z + c
