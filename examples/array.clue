local array = [1, 2, 3, 4]
assert(array[0] == 1)

table.insert(array, "hello")
table.insert(array, "world")
print(array[4], array[5])

for i = 0, #array {
	print(array[i])
}
