from fertilizer import Range, MultiRange

a = Range(0, 20)
b = Range(14, 40)
c = Range(60, 30)

print(a + c + b)
print((a / c) == MultiRange())
