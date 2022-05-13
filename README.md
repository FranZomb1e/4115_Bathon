# Bathon

### File Tree

- scanner.mll: scanner
- bathonparse.mly: parser
- ast.ml: abstract syntax tree
- semant.ml: semantic checker
- sast.ml: semantically checked abstract syntax tree
- irgen.ml: IR generation
- builtin.c: builtin C functions and libraries.
- test1.ml: test scanner and parser
- test2.ml: test semantic checker
- test3.ml: test IR (llvm) generation
- test.sh: (**hard-coded for testing gcd.ch**) shell script to generate .native and .out files for testing
- test_print.sh: shell script to test test-print.ch
- test_list_func.sh: shell script to test test-list-func.ch
- test_list_fund.sh: shell script to test test-list-fund.ch
- test_bash_fund.sh: shell script to test test-bash-fund.ch
- testcases/  
  - gcd.ch: naive greatest common divisor algorithm
  - test-bash-fund.ch: basic bash command 'ls'.  
  - test-list-fund.ch: basic list methods such as append 
  - test-list-func.ch: pass list as a parameter of a function
  - test-print.ch: print
- README.md

### Testing

We would use the gcd test case **./testcases/gcd.ch** for the following illustrations.

```
# The GCD algorithm in Bathon
x : int
y : int

def gcd : int (a : int, b : int) {
  while (a != b) {
    if (b < a) {
      a = a - b
    } else { 
      b = b - a 
    }
  }
  return a
}

a = 18
b = 9
x = 2
y = 14
print(gcd(x,y))
print(gcd(3,15))
print(gcd(99,121))
print(gcd(a,b))
```

#### test1

test1 for scanner and parser.

We first build test1.native and then run it:

```
ocamlbuild test1.native
./test1.native
```

Once test1.native is running, we can run tests by copying and pasting the source code of the test cases (e.g. testcases/gcd.ch) into terminal, then press **ctrl + d** (assume macOS is used). And the parsed program would be printed in terminal.

example output (testcases/gcd.ch):

```
Parsed program: 

x : int[]

def test : int(a)
{
append(a, 1)
return a[1];
}


init(x)

append(x, 0)

print(x[0])

a = x

print(test(a))
```

#### test2

test2 for semantic checker.

Build and run test2.native:

```
ocamlbuild test2.native
./test2.native
```

Copy and paste the source code of test cases into terminal, then press **ctrl + d**. And the semantically checked program would be printed.

example output (testcases/gcd.ch):

```
Semantically checked program: 

y : int

x : int

def gcd : int(a, b)
{
locals : 
while ((bool : (int : a) != (int : b)))
{
if ((bool : (int : b) < (int : a)))
{
(int : a = (int : (int : a) - (int : b)))
}
else
{
(int : b = (int : (int : b) - (int : a)))
}
}
return (int : a)
}

def main : int()
{
locals : a, b
(int : a = (int : 18))
(int : b = (int : 9))
(int : x = (int : 2))
(int : y = (int : 14))
(int : print((int : gcd((int : x),(int : y)))))
(int : print((int : gcd((int : 3),(int : 15)))))
(int : print((int : gcd((int : 99),(int : 121)))))
(int : print((int : gcd((int : a),(int : b)))))
}
```

#### test3

test3 for IR generation and final executable.

llvm is required for test3. **If llvm not installed**, may need to run the following commands:

```
brew install llvm
opam install llvm.13.0.0
```

run the following script to generate test3.native and a.out:

```
./test.sh
```

test for IR generation:

```
./test3.native
```

example output - generated llvm IR (testcases/gcd.ch):

```
; ModuleID = 'Bathon'
source_filename = "Bathon"

%list = type { i8**, i32, i32, i8* }

@y = global i32 0
@x = global i32 0
@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1
@fmt.3 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.4 = private unnamed_addr constant [4 x i8] c"%g\0A\00", align 1
@fmt.5 = private unnamed_addr constant [4 x i8] c"%s\0A\00", align 1

declare i32 @printf(i8*, ...)

declare i8* @exec(i8*, ...)

declare %list* @create_list(i8*)

declare i32 @access_int(%list*, i32)

declare float @access_float(%list*, i32)

declare i8* @access_str(%list*, i32)

declare i32 @append_int(%list*, i32)

declare i32 @append_float(%list*, float)

declare i32 @append_str(%list*, i8*)

declare i32 @assign_int(%list*, i32, i32)

declare float @assign_float(%list*, i32, float)

declare i8* @assign_str(%list*, i32, i8*)

define i32 @gcd(i32 %a, i32 %b) {
entry:
  %a1 = alloca i32, align 4
  store i32 %a, i32* %a1, align 4
  %b2 = alloca i32, align 4
  store i32 %b, i32* %b2, align 4
  br label %while

while:                                            ; preds = %if_end, %entry
  %a3 = load i32, i32* %a1, align 4
  %b4 = load i32, i32* %b2, align 4
  %tmp = icmp ne i32 %a3, %b4
  br i1 %tmp, label %while_body, label %while_end

while_body:                                       ; preds = %while
  %b5 = load i32, i32* %b2, align 4
  %a6 = load i32, i32* %a1, align 4
  %tmp7 = icmp slt i32 %b5, %a6
  br i1 %tmp7, label %then, label %else

then:                                             ; preds = %while_body
  %a8 = load i32, i32* %a1, align 4
  %b9 = load i32, i32* %b2, align 4
  %tmp10 = sub i32 %a8, %b9
  store i32 %tmp10, i32* %a1, align 4
  br label %if_end

else:                                             ; preds = %while_body
  %b11 = load i32, i32* %b2, align 4
  %a12 = load i32, i32* %a1, align 4
  %tmp13 = sub i32 %b11, %a12
  store i32 %tmp13, i32* %b2, align 4
  br label %if_end

if_end:                                           ; preds = %else, %then
  br label %while

while_end:                                        ; preds = %while
  %a14 = load i32, i32* %a1, align 4
  ret i32 %a14
}

define i32 @main() {
entry:
  %a = alloca i32, align 4
  %b = alloca i32, align 4
  store i32 18, i32* %a, align 4
  store i32 9, i32* %b, align 4
  store i32 2, i32* @x, align 4
  store i32 14, i32* @y, align 4
  %y = load i32, i32* @y, align 4
  %x = load i32, i32* @x, align 4
  %gcd_result = call i32 @gcd(i32 %x, i32 %y)
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i32 %gcd_result)
  %gcd_result1 = call i32 @gcd(i32 3, i32 15)
  %printf2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i32 %gcd_result1)
  %gcd_result3 = call i32 @gcd(i32 99, i32 121)
  %printf4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i32 %gcd_result3)
  %b5 = load i32, i32* %b, align 4
  %a6 = load i32, i32* %a, align 4
  %gcd_result7 = call i32 @gcd(i32 %a6, i32 %b5)
  %printf8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.3, i32 0, i32 0), i32 %gcd_result7)
  ret i32 0
}
```

test for final executable:

```
./a.out
```

example output - final executable (testcases/gcd.ch):

```
2
3
11
9
```

#### testing for other test cases

we write an individual testing shell script for each test case, e.g. 

test-print.ch:

```
x : int
y : float
z : bool
a : str

x = 1
print(x)
print(2)
y = 1.1
print(y)
print(2.2)
z = True
print(z)
print(False)
a = "1s"
print(a)
print("2s")
```

test by:

```
./test_print.sh
./a.out
```

example output:

```
1
2
1.1
2.2
1
0
1s
2s
```

And similarly others:

```
./test_list_func.sh
./a.out
```

```
./test_list_fund.sh
./a.out
```

```
./test_bash_fund.sh
./a.out
```

