@echo off
mkdir build 2> nul
pushd "build"
cl.exe /nologo /Zi /MTd /W4 /wd4530 /wd4201 /wd4577 /wd4310 /FC /std:c++17 /GR- ../Vulpes.cpp /link user32.lib Shell32.lib Kernel32.lib /MACHINE:X64 /out:"vulpes.exe"
popd