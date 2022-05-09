# test for commands

# Easy to run command
# Simply run a command
`ls > dirlist 2>&1`

# List all the files in the current directory
ls_res = `ls`
for file in ls_res.split("\n") {
    print(file)
}

# Easy print with variable
x = 1
print(x)                           # 1


