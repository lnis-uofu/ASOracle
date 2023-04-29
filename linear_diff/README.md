linear_diff
===========

Implements a domain specific language for LDEs.

```
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

# Expressions
a = 1
b = a + y
c = 1 + 2 * 5
z = integrate(-4.5 * x + 2)
w = 5 * y + - z + c
```
