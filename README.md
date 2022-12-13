# ParVarys

## 📋 Description

A parallel implemention of the Varys algorithm coded in Haskell.

To learn more about the Varys algorithm see this paper:

https://dl.acm.org/doi/10.1145/3230543.3230569

## ✍️ Authors

Created by **Etesam Ansari** & **YunLan Li**

## 📦 Running The Project

```bash
stack install
```

```bash
stack build
```

```bash
stack exec ParVarys-exe
```

## ➕ Adding Packages

- To add a package I think you have to first add it to **extra-deps section** of the stack.yaml
- Then you have to add it to the **dependencies** section of the package.yaml
  - You may also want to add it to the executable dependencies section

## 📝 Installing threadscope

### Command To Run

```bash
stack install --flag gtk:have-quartz-gtk threadscope
```

### Testing it works

```bash
threadscope --test ch8
```

This should launch a window

For more information see this: https://wiki.haskell.org/ThreadScope

### Trying a more involved example

We want to run the hellofib function in parallel

```bash
cabal install --lib parallel
```

#### Create the executable

```bash
ghc -O2 -rtsopts -eventlog -threaded src/hellofib.hs
```

#### Run the executable in parallel

This command runs the program with 4 cores.

```bash
./src/hellofib +RTS -N4 -l
```

To see the threadscope output

```bash
threadscope hellofib.eventlog
```
