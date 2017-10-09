

# run only from `nix-shell --argstr compiler ghcjs`
# shell brings the closure compiler if not otherwise present

mkdir ./static/js
mkdir ./static/themeExFou.jsexe/
mkdir ./static/themeExSem.jsexe/
mkdir ./static/themeExBs.jsexe/
mkdir ./static/themeExW3.jsexe/

cp -R ./dist-newstyle/build/x86_64-linux/ghcjs-0.2.1/themeExsFou-0.1.0.0/c/themeExFou/build/themeExFou/themeExFou.jsexe/* ./static/themeExFou.jsexe/
cp -R ./dist-newstyle/build/x86_64-linux/ghcjs-0.2.1/themeExsW3-0.1.0.0/c/themeExW3/build/themeExW3/themeExW3.jsexe/* ./static/themeExW3.jsexe/
cp -R ./dist-newstyle/build/x86_64-linux/ghcjs-0.2.1/themeExsSem-0.1.0.0/c/themeExSem/build/themeExSem/themeExSem.jsexe/* ./static/themeExSem.jsexe/
cp -R ./dist-newstyle/build/x86_64-linux/ghcjs-0.2.1/themeExsBs-0.1.0.0/c/themeExBs/build/themeExBs/themeExBs.jsexe/* ./static/themeExBs.jsexe/

echo "themeExBs"
cd static/themeExBs.jsexe
closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > ../js/themeExBs.min.js

echo "themeExFou"
cd ../themeExFou.jsexe
closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > ../js/themeExFou.min.js

echo "themeExSem"
cd ../themeExSem.jsexe
closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > ../js/themeExSem.min.js

echo "themeExW3"
cd ../themeExW3.jsexe
closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > ../js/themeExW3.min.js
