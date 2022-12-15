# ParVarys

## 📋 Description

A parallel implemention of the Varys algorithm coded in Haskell.

To learn more about the Varys algorithm see this paper:

https://dl.acm.org/doi/10.1145/2619239.2626315

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

This command runs the ParVarys executable using 4 cores, and generates the
event log `ParVarys-exe.eventlog`:

```bash
stack exec ParVarys-exe -- +RTS -N4 -lf
```

To visualize the event log using threadscope

```bash
threadscope ParVarys-exe.eventlog &
```
