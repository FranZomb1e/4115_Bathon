# test for commands

# Easy to run command
# Simply run a command
`ls > dirlist 2>&1`

# List all the files in the current directory
ls_res = `ls`
for file in ls_res.split("\n") {
    print(file)
}

if ($# > 2) {
    print("The number of arguments is less than two")
}
`echo "The first argument is $1"`
`echo "The second argument is $2"`

# Easy print with variable
x = 1
print(x)                           # 1
print("x variable is equal to $x") # x variable is equal to 1
# print("$x1")                       # Error
print("$x ")                       # 1<whitespace>
print("${x}1")                     # 11
print("\$x")                       # $x

# Easy pattern match with regex
regex = "[0-9]+\.[0-9]+[[e|E][-|+]?[1-9][0-9]*]*"
print("1.2E+8" match regex) # [(0, 6)]
print(".2E+8" match regex)  # []
print("1.2E+8 1e3" match regex) # [(0, 6), (7, 3)]
