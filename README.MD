# Pacman in Haskell
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2FBenStokmans%2Fpacman-hs.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2FBenStokmans%2Fpacman-hs?ref=badge_shield)


## Setup development environment (Windows)

```sh
# Install dependencies
stack exec -- pacman -Syu

# We have to download an older version of sdl2 because the newest doesn't work with our bindings
stack exec -- curl -O https://repo.msys2.org/mingw/x86_64/mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst
stack exec -- pacman -U mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst
rm .\mingw-w64-x86_64-SDL2-2.0.22-2-any.pkg.tar.zst

stack exec -- pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2_ttf mingw-w64-x86_64-SDL2_image mingw-w64-x86_64-SDL2_mixer

# Setup stack
stack setup

# Build and run the game (using this command instead of stack run makes sure we use glfw and don't need GLUT dependencies)
stack build --flag gloss:glfw --flag gloss:-glut --fast :pacman-exe --exec pacman-exe
```

## Setup environment (Debian)
```sh
# Install dependencies
sudo apt-get install libsdl2-dev libsdl2-ttf-dev libsdl2-image-dev libsdl2-mixer-dev

# Setup stack
stack setup

# Build and run the game (using this command instead of stack run makes sure we use glfw and don't need GLUT dependencies)
stack build --flag gloss:glfw --flag gloss:-glut --fast :pacman-exe --exec pacman-exe
```

## License
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2FBenStokmans%2Fpacman-hs.svg?type=large)](https://app.fossa.com/projects/git%2Bgithub.com%2FBenStokmans%2Fpacman-hs?ref=badge_large)