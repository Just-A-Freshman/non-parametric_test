X = [(2, 4, 8), (6, 9, 10), (5, 3, 1)]
Y = [(1, 6, 3), (7, 2, 1), (6, 3, 3)]

def avg(x):
    return sum(x) / len(x)

new_X = [sum([(i-avg(x))**2 for i in x]) for x in X]
new_Y = [sum([(i - avg(y))**2 for i in y]) for y in Y]

print(new_X)


print(new_Y)