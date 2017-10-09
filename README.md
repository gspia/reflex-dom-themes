# reflex-dom-themes

This library uses 
[css2HsClassName](https://github.com/gspia/css2HsClassName)
to produce constants that can be used to build theme-libs building
that are based on 
[reflex-dom-htmlea](https://github.com/gspia/reflex-dom-htmlea).

Why? Because it is too easy to make typo while making an attribute map 
with appropriate class-contents. By using predefined constants, compiler
will tell if the name is misspelled. This thus helps to avoid 
runtime debugging.

Why many css- and icon libs in one place? No good excuses nor 
explanations for this one.
(Maybe if willing to try and compare or something like that?) 

This is WIP ATM and will probably stay such for a while.


## How to build

After git cloning, start a shell
```
nix-shell
```
Then you can build examples with
```
cabal new-build all
```
which builds webkit2gtk and wai-apps. 
```
./cpexesTMP.sh
```
copies the executables from dist-newbuild-directories to bin-directory. After 
that you can just `./bin/themeExBs-wai`, which starts a server at localhost:8000.
(Note that `-kits` don't show the themes and the fonts for semantic wai-app are
not ok atm.)

Alternatively, you can start shell with
```
nix-shell --argstr compiler ghcjs
```
and then
```
cabal new-configure --ghcjs
cabal new-build all
```
which builds the js-sources. Command `./cpexejssTMP.sh` copies all 
examples to static-directory and runs closure-compiler on them. 
Command `./cpbsjsTMP.sh` does that only to bootstrap-example. Note that 
closure-compiler is provided as a build tool for the shell env and thus 
these last two copy commands work at the nix-shell.

After that you can either open browser with `static/indexBs.html` or start 
a warp or some other server and open it at localhost. Note that 
indexBsS.html is for jsaddle-wai-trials. Similarly for the other themes 
(files with large S, that is, indexFouS etc.,  are for jsaddle-wai).

Command 
```
nix-build
```
makes just the lib.


## How to use dev-server

Go to the nix-shell and then type
```
./dev-server.sh
```

This starts the ghcid that is tracking the source changes. It points to 
bootstrap-version. It can be changed at .ghci-file. 

See also [jsaddle](https://github.com/ghcjs/jsaddle).


## Caveats, bugs

This lib is probably going to go through some re-organization 
and such things. 


## Related work

See github, and especially

- [reflex-dom-semui](https://github.com/reflex-frp/reflex-dom-semui)
- [semantic-reflex](https://github.com/tomsmalley/semantic-reflex)
- [reflex-material](https://github.com/alasconnect/reflex-material)
- [reflex-material-boostrap](https://github.com/hexresearch/reflex-material-bootstrap)

Some of the above libs are actively developed so be sure to check them!

