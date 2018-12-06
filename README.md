# NDTM
A non-deterministic turing machine written in Haskell.

# Build
```
stack init 
stack build
```

# Usage

Takes a turing machine descriptor and input files as command line arguments.
```
stack exec TuringMachineEmulator <descriptor filename> <input filename>
stack exec TuringMachineEmulator machines/ndPalindrome inputs/p 
```


# To run tests
```
cabal test
cat dist/test/TuringMachineEmulator-0.1.0.0-spec.log
```
