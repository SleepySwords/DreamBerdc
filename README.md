# [dreamberdc](https://github.com/TodePond/dreamberd)

An attempt to make my first compiler, why not make it perfect.

This compiler is able to generate LLVM IR, which can then be used to generate
object files and linked to create a binary. This binary file also contains debug
symbols so you can use debuggers such as GDB and LLDB to step through the source
code you have written.

Some features of this compiler
- Classes (not safe...)
- Functions
- Basic typing
- Variables
- Pointers
- External function binding support (via extern)

## Usage

### via cargo using the source

```
cargo run -- <input>
```

### via binary using the source

```
dreamberdc <input>
```

## Options (ran by the --help command)

```
Options:
  -o, --output <OUTPUT>              
  -m, --mode <MODE>                  [default: jit] [possible values: jit, llvmir, object]
  -l, --log-info                     
  -O, --optimisation <OPTIMISATION>  [default: none] [possible values: none, less, default, aggresive]
  -h, --help                         Print help
  -V, --version                      Print version
```

There are some files in `examples/` to play with.
