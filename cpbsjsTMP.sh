

# run only from nix-shell --argstr compiler ghcjs

mkdir ./static/themeExBs.jsexe

cp -R ./dist-newstyle/build/x86_64-linux/ghcjs-0.2.1/themeExsBs-0.1.0.0/c/themeExBs/build/themeExBs/themeExBs.jsexe/* ./static/themeExBs.jsexe/

echo "themeExBs"
cd static/themeExBs.jsexe
closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > ../js/themeExBs.min.js
