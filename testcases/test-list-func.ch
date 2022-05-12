x : int[]

def test : int (a : int[]) {
    append(a, 1)
    return a[1]
}

init(x)
append(x, 0)
print(x[0])
a = x
print(test(a))
