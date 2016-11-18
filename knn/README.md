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
