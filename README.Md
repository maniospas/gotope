# `tope

An small interpreted language with massive parallelization.

## :zap: Hello world!

Currently, only building from source is avialable. It requires
a C++ compiler, like gcc. Clone this repository and compile ts
contents like below, where the *openmp* compilation flag is required
for parallel execution:

```bash
g++ -O2 gotope.cpp -o gt -fopenmp
```

Now create a simple file and use `out|` to stream a string to
the console's output resource. Compile and run the program like
below in the default CPU-based virtual machine. This uses threaded
parallelization
Notice how the result counts the number of parallelized
iterations.

```
// main.gt
out| "Hello world!"
```
```bash
> ./gt main.gt
Hello world!
Done (1 loops)

```


## :rocket: Syntax

The computational model is that you write programs as
a series of intructions that look high-level but are
lowered into operations of a very simple virtual machine.

There are two kinds of constructs: scopes that start and end
with brackets `name {code}` and variables that are declared
with `name:` followed by an operation. Operations, where arguments
are other variables, numbers, or (sometimes) strings:

- `=arg`
- `*(arg1,arg2)`
- `+(arg1,arg2)`
- `^(arg1,arg2)`
- `<(arg1,arg2)`

Print per `out|value`. The `|` operator is used to stream data
into a resource, where the console output
resource `out` is declared by default.
Streamed data are updated at corresponding resources
while the program runs.

Create structured scopes by writing a name, and then starting
and closing brackets `{}`.
You can reference variables in scoped names with the dot
notation `scope.var`. Finally, use the `'` symbol to print
specific values. Use `//` for line comments. Here is an example
(use a c highlighter):

```c
// main.gt
point {
    x:=10
    y:=20
}
out| point.x  // prints 1
```

```
> gt main.gt
10
Done (1 loops)
```

