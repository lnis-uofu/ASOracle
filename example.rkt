#lang linear_diff
# Setup
design test
eia-96
vdd = 9
vss = -9.0
gnd = 0

input a, b
input c
output x
output z, y


x = 5
y = x + 2

d = integrate(-34*a + x*4)
z = 4*d
