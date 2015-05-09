# Enter your code here. Read input from STDIN. Print output to STDOUT
n = int(raw_input())
p = int(raw_input())

def squareRoot(n, p):
    a = 5 * n
    b = 5
    limit = 10 ** (p + 1)
    while b < limit:
        if (a >= b):
            a -= b
            b += 10
        else:
            a *= 100
            b = (b - 5) * 10 + 5
    return b / 100

def sumDigits(n):
   r = 0
   while n:
       r, n = r + n % 10, n / 10
   return r

def sqrtFloat(n, p):
    from decimal import *
    getcontext().prec = p
    return Decimal(n).sqrt()

def perfectRoot(x):
    import math
    sqrt = int(math.sqrt(x))
    return sqrt * sqrt == x

irrational = [ x for x in range(1, n + 1) if not perfectRoot(x) ]
digits = [ sum(sqrtFloat(m, p + 10).as_tuple()[1][:p]) for m in irrational ]
print(sum(digits))
