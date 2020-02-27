This is a CMake branch of iMOD

Use ninja from https://github.com/Kitware/ninja/releases

From an Intel Compiler prompt:
```
mkdir build
cd build
cd .. && rm -rf build && mkdir build && cd build
cmake -GNinja -DCMAKE_C_COMPILER=icl -DCMAKE_Fortran_COMPILER=ifort ..
cmake --build .
```
