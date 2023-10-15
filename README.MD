# Install dependencies
stack exec -- pacman -Syu
stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_ttf

# Download and place next to exe
# - https://www.libsdl.org/release/SDL2-devel-2.28.4-mingw.tar.gz
# - https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-devel-2.20.2-mingw.tar.gz

stack setup
stack run
stack build --file-watch