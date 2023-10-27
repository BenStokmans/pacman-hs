stack build --flag gloss:glfw --flag gloss:-glut --ghc-options '-O2 -funfolding-use-threshold=16 -fexcess-precision -optc-O3 -optc-ffast-math' --copy-bins --local-bin-path "./build"
cp -r assets/ build/assets
cp -r maps/ build/maps