# [dreamberdc](https://github.com/TodePond/dreamberd)

An attempt to make my first compiler, why not make it perfect.

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

There are some files in `test/` to play with.
