# Install dependencies
stack exec -- pacman -Syu

# have to download older version because newest version is broken
stack exec -- curl -O https://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst
stack exec -- pacman -U mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst
rm .\mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst

stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2_ttf

# Download freeglut
https://www.transmissionzero.co.uk/files/software/development/GLUT/freeglut-MinGW.zip
move `freeglut-MinGW-3.0.0-1.mp.zip\freeglut\bin\x64\freeglut.dll` into the root of the project

stack setup
stack run