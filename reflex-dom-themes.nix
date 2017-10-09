{ mkDerivation, base, containers, data-default
  , stdenv, text, reflex, reflex-dom-core, ghcjs-dom
  , wai-app-static, warp, wai-middleware-static, wai, websockets 
  , jsaddle, jsaddle-warp 
  , ghcjs-base 
  , jsaddle-webkit2gtk 
  , reflex-dom-htmlea 
  , compiler ? "ghc"
}:
# So, either ghcjs-base or jsaddle-webkitgtk
# ? (import <nixpkgs> {}).haskellPackages.reflex-dom-htmlea
mkDerivation {
  pname = "reflex-dom-themes";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers data-default text
    ghcjs-dom jsaddle reflex reflex-dom-core
    reflex-dom-htmlea 
  ];
  executableHaskellDepends = [
    base containers ghcjs-dom jsaddle reflex
    reflex-dom-core text
    reflex-dom-htmlea
  ] ++ (if compiler == "ghc"
          then [jsaddle-webkit2gtk jsaddle-warp warp 
            wai-app-static wai-middleware-static websockets] 
          else [ghcjs-base]);
  description = "Class names of various css libs and icon sets";
  license = stdenv.lib.licenses.bsd3;
}


