# Syntax
## Function definitions
```
let fun = {
    Num a: a x, a y -> a =>
    let a = x * x;
    a + y
}
```
If last the line in a function body does not have semicolon it will be returned. If the last line in a function body has return signature () then semicolon is optional.

## Closures
```
let main = {
    let min = 1.0
    foos
    map get_value
    filter { Foo f -> bool =>
        f >= min
    }
    first
    print
}

// Typed closure, takes input
{
    Foo -> Bar =>
    let t = foo.get;
    bar t
}

There is no distinction between a closure and a function.

// Untyped closure, does not take input
{
    foo do_stuff
}
```

## Types
Tuple types
```
type Coord = (float, float);
```
Sum types
```
type Num = int | float | double;
```
List types
```
[A]
```
Void type
```
()
```
Function signatures
```
A: [A] list -> int // takes a list of any element and returns an int
Num n: [n] list, n identity -> n // takes a list of type n and a single element of type n and returns a value of type n
```
Sum types are evaluated at compile-time. For any combination of variants any single function must be guaranteed to return exactly one type at compile time.

## Function calls
```
let foo = {
    Bar bar, int i -> bool =>
    bar[0] == i
}

let main = {
    let i = 0;
    bar foo i;
    foo bar i
}
```
Function calls are done without paranthesis. Arguments are simply listet after the function name. Function composition is done by listing a function name after the list of arguments . is not needed. To separate arguments function composition putting the function on a new line is recommended.
## Tuple constructor
```
type Triple = (A, B, C);

let new_triple = {
    () -> Triple =>
    let a = new_a;
    let b = new_b;
    let c = new_c;
    (a, b, c)
}
```

## Accessing tuple members
```
type Triple = (A, B, C);

let get_a = {Triple t -> A =>
    t[0]
}

let get_b = {Triple t -> B =>
    t[1]
}

let get_c = {Triple t -> C =>
    t[2]
}

let main = {
    Triple t = new_triple;
    t get_a print;
    t get_b print;
    t get_c print;
}
```
All fields in tuples are anonymous. To access a field by name a function is needed.

## Conditionals
```
let abs = {Num a: a val -> a =>
    let mut ret = a
    (a < 0) then {
        ret = -a
    }
    ret
}
```

# Inbuilt functions
## Midfix functions
```
(+) Addition
(-) Subtraction
(*) Multiplication
(/) Division
(==) Equals
(!=) Not Equals
(>) Greater
(>=) Equal or greater
(<) Less
(<=) Equal of less
(&&) Logical and
(||) Logical or
(!) Logic negation
(|) Bitwise or
(&) Bitwise and
(^) Bitwise xor
(~) Bitwise negation
```
## Math functions
```
let mod = {...}

let div = {...}

let pow = {...}

let divides = {
    Num a: a d, a n -> bool =>
    div n d == 0
}
```
## Conditionals
```
// If boolean value is true, performs function. Returns value of preicate
let then = {
    bool pred, (() -> ()) fun -> bool =>
    ...
}

// If boolean value is false, performs function
let else = {
    bool pred, (() -> ()) fun =>
    ...
}

// Returns true if pred1 is false and pred2 is true, otherwise returns false
let or = {
    bool pred1, bool pred2, -> bool =>
    !pred1 && pred2
}
```

## List functions
```
// Gets length of list
let len = {
    A: [A] list -> int =>
    ...
}

// Accesses element at random index in list
list[i]

// Creates an iterator with range [start, end>
let to = {
    int start, int end -> [int] =>
    ...
}

// Performs function on every element in list
let map = {
    A: [A] list, (A -> A) fun -> [A] =>
    ...
}

// Performs function on every element in list, returning nothing
let foreach = {
    A: [A] list, (A -> ()) fun =>
    ...
}

// Takes a list and returns a new list where every 
// element is from the first, but predicate evaluates to true
let filter = {
    A: [A] list, (A -> bool) pred -> [A] =>
    ...
}

// Returns first element of list
let first = {
    A: [A] list -> A =>
    list[0]
}

// Reduces the list by left-fold
let reduce = {
    A: [A] list, (A, A -> A) -> A =>
    ...
}

// Finds first element of list which satisfied predicate
let find = {
    A: [A] list, (A -> bool) pred -> [A] =>
    list filter pred first
}
```

## General functions
```
let print = {
    Printable p =>
    ...
}

let error = {
    Printable p =>
    ...
}
```

# Examples
## Fizzbuzz
```
let main = {
    0 to 100
     foreach {
        int n =>
        15 divides n then {
            print "fizzbuzz"
        }
        or {5.divides(n)} then {
            print "buzz"
        }
        or {3.divides(n)} then {
            print "fizz"
        }
        else {
            print n 
        }
    }
}
```