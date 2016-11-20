# `mlalgs`

Examples of basic machine learning algorithms

# Install prerequisites

## Windows

```cmd
stack exec -- pacman -Syu
stack exec -- pacman -Sy mingw64/mingw-w64-x86_64-lapack mingw64/mingw-w64-x86_64-openblas
```

## Ubuntu

```bash
sudo apt-get install libblas-dev liblapack-dev
```

## Centos

```bash
sudo yum install blas-devel lapack-devel
```

# Build

```bash
stack build
```

# Setting up runtime environment

On Windows, you'll need to run `script\env.cmd` (Windows command prompt) or `script\env.ps1` (PowerShell) to set up the `PATH` environment variable so that various libraries (e.g. ICU) can be located at runtime:

```cmd
script\env.cmd
```

or

```ps
script\env.ps1
```
