# Perun Programming Language

Perun is a toy programming language created to teach myself LLVM.

## Hello World Application

This is an example hello world application.

```zig
extern fn puts(str: rawptr): i32;

fn main() : i32 {
    puts("Hello World!");
    return 0;
}
```

To compile it enter the following commands:
```bash
$ perun hello.pr -o hello.o
$ gcc hello.o -o hello

```

And you can run it with:
```bash
$ ./hello
Hello World
```

To learn the general syntax please check the examples.