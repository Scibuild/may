# May

Please ensure to clone with `--recurse-submodules`.

## Dependencies
- `opam`: for OCaml dependency management
- `qbe`
- An assembler and linker

## Building

With `opam` installed, we need to manually install `dune` and `opam-monorepo`. 
Then we use `monorepo` to pull in the remaining source dependencies.

```sh
opam install dune
opam install monorepo
opam monorepo pull
```

Once this has completed then we can build and run tests with:
```
dune build @all @runtest
```

The final executable is `may.exe` and can be invoked with :

```./may.exe <SOURCE_FILE> [-o OUPTUT_QBE]```

## Running May Programs

The May compiler outputs `qbe` source which can be compiled to assembly using [qbe](https://c9x.me/compile/).
Then feel free to use your favourite assembler-linker toolchain to produce a compiled executable, making sure to link against `_build/default/runtime/libmayruntime.a`.
An example `Makefile` is present in `test/programs/Makefile`.

## Examples

See `test/programs/*.may` or the `test_qbe_backend.ml` and `test_bytecode_backend.ml` files for examples.