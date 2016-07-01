# cpp_codegen
Trivial C++ code generating and formatting tools written in Haskell.

## gentest
This generates a stub Google Test file from a .cpp or .h file.  
See `test/sample.cpp` and `test/sample.t.cpp` for a preview.

## annotate_bregs
This is a work in progress.

## TODO
* Make gencpp parse [FilePath] from cmdline
* Lookup BREG# values from their deployed headers
* Replace `String` with `Data.Text` and make Gentest use Turtle
