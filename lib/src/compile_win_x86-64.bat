@echo off
echo I am going to compile the sources to libsvm.dll for the Windows x86-64 plattform.
echo Assuming that you have installed Visual C++ (Express) in the following directory:
echo C:\Program Files\Microsoft Visual Studio 8
echo And the Microsoft .NET SDK in:
echo C:\Program Files\Microsoft SDKs
echo 

set INCLUDE=C:\Program Files\Microsoft Visual Studio 8\VC\include\;
set LIB=C:\Program Files\Microsoft Visual Studio 8\VC\lib;C:\Program Files\Microsoft SDKs\Windows\v6.0A\Lib\x64;

if exist %svm.h goto headerExists
echo Problem: svm.h not found!
goto ABORT

:headerExists
echo Header file found.
goto compile

:compile
cl /LD /I svm.h svm.cpp
echo Compilation successful. Output file: svm.dll (for x86-64).
goto END

:ABORT
echo !!!Program aborted, compiled nothing!!!
goto END

:END
