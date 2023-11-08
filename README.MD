# Install dependencies
stack exec -- pacman -Syu

# have to download older version because newest version is broken
stack exec -- curl -O https://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst
stack exec -- pacman -U mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst
rm .\mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst

stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_image mingw-w64-x86_64-SDL2_mixer

stack setup
stack run