from fertilizer import Range, MultiRange

a = Range(0, 20)
b = Range(14, 40)
c = Range(60, 30)
d = Range(51, 10)
e = Range(0, 120)

print(b+c)
print((a / d) == MultiRange())
print(len(b+c))

print((a+b) / (c+d))
print(a+d)
print(b+c)
print((a+d) / (b+c))

print(a - b)
print(d - b)
print(e-a)
