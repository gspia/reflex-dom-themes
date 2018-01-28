# reflex-dom-themes

This library uses 
[css2HsClassName](https://github.com/gspia/css2HsClassName)
to produce constants that can be used to build theme-libs building
that are based on 
[reflex-dom-htmlea](https://github.com/gspia/reflex-dom-htmlea).

This is WIP ATM and will probably stay such for a while.


## How to build

First, get the repo with `git clone` and `cd` into the directory, and after that make sure that the reflex-platform is in place:

```
git submodule update --init --recursive
```

To build with GHC, use the nix-shell command to enter the sandbox shell and use cabal (which is supplied by the sandbox). This builds webkit2gtk-version but 
jsaddle-warp can be used as well.

```
nix-shell -A shells.ghc
cabal new-build all
```

To build with GHCJS:

```
nix-shell -A shells.ghcjs
cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all
```

You can also build examples separately by replacing all with exe:name, e.g.

```
cabal new-build exe:tableEx
cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build exe:exampleTbl
```

For further information, see the following
- [project-development documentation](https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md)
- [blanket project derivation (default.nix)](https://github.com/reflex-frp/reflex-platform/blob/develop/project/default.nix)
- [reflex-project-skeleton](https://github.com/ElvishJerricco/reflex-project-skeleton)

Note that if you have already obtained repo but want to update the 
reflex-platform, you can try, e.g.,

```
git submodule foreach "(git checkout develop; git pull --recurse-submodules)&"
```

(Note that the above command gets the develop-branch of the platform.)

## TODOs

The wish list is quite long at the moment. The list can be seen
at [todo.md](./todo.md).
It includes larger ones that probably take longer time and things that are 
on to-be-done-next list and on would-be-nice-to-have list.


## Caveats, bugs

The webkit2gtk-versions are not working properly, because most of the css-libs
assume some js and, at the moment, we have not implemented the corresponding
functionality based on frp. 

This lib is probably going to go through some re-organization 
and such things. 


## Related work

See github, and especially

- [reflex-dom-semui](https://github.com/reflex-frp/reflex-dom-semui)
- [semantic-reflex](https://github.com/tomsmalley/semantic-reflex)
- [reflex-material](https://github.com/alasconnect/reflex-material)
- [reflex-material-boostrap](https://github.com/hexresearch/reflex-material-bootstrap)

Some of the above libs are actively developed so be sure to check them!

