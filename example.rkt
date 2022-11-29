#lang linear-diff
# Setup
design test
eia-96
vdd = 9
vss = -9.0
gnd = 0

# Expressions
input y
input x
output w
output z

z = integrate(-4.5 * x + 2)
w = 1.2 * y + - z + x
