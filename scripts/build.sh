stack build --flag gloss:glfw --flag gloss:-glut --ghc-options -O2 --copy-bins --local-bin-path "./build"
cp -r assets/ build/assets
cp -r maps/ build/maps