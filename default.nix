{ reflex-platform ? import ./reflex-platform.nix
, compiler ? "ghcjs"
} :
let
  initialNixpkgs = import <nixpkgs> {};

  pkgs = reflex-platform.nixpkgs.pkgs;
  hpkgs = initialNixpkgs.pkgs.haskellPackages;
  hLib = initialNixpkgs.haskell.lib;

  adjust-for-ghcjs = drv: {
    /* executableSystemDepends = [ ]; */
    buildDepends = [
      pkgs.closurecompiler 
      hpkgs.cabal-install
      hpkgs.hlint
      hpkgs.pointfree
      hpkgs.pointful
    ];
    executableToolDepends = [pkgs.closurecompiler pkgs.zopfli];
    doHaddock = false;
    postInstall = ''
      mkdir -p $out
      mkdir -p $out/css
      mkdir -p $out/figs
      mkdir -p $out/js
      cp ./assets/indexBs.html  $out/indexBs.html
      cp ./assets/indexFou.html $out/indexFou.html
      cp ./assets/indexSem.html $out/indexSem.html
      cp ./assets/css/fou.css $out/css/fou.css
      cp ./assets/css/sem.css $out/css/sem.css
      cp ./assets/css/semantic.min.css $out/css/semantic.min.css
      cp ./assets/js/what-input.min.js $out/js/what-input.min.js
    '';
  };
      /* cp $out/bin/themesBs.jsexe/all.js  $out/js/themesBs.js */
      /* cp $out/bin/themesFou.jsexe/all.js $out/js/themesFou.js */
      /* cp $out/bin/themesSem.jsexe/all.js $out/js/themesSem.js */
      /* cd $out/bin/themesBs.jsexe */
      /* closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/themesBs.min.js */
      /* cd $out/bin/themesFou.jsexe */
      /* closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/themesFou.min.js */
      /* cd $out/bin/themesSem.jsexe */
      /* closure-compiler all.js --compilation_level=ADVANCED_OPTIMIZATIONS --isolation_mode=IIFE --assume_function_wrapper --jscomp_off="*" --externs=all.js.externs > $out/js/themesSem.min.js */
    /* ''; */
  /* }; */
  #    rm -Rf $out/bin/themesEx.jsexe
  #    rm -Rf $out/bin
  #    cd $out/js
  #    gzip themesEx.min.js
  #    zopfli -i1000 themesEx.min.js
  #installPhase = ''
  #  mkdir $out
  #  cp -r ./* $out/
  #'';
  adjust-for-ghc = drv: {
    executableSystemDepends = [
      reflex-platform.${compiler}.ghcid
      reflex-platform.${compiler}.cabal-install
    ];
    buildDepends = [
      hpkgs.ghc-mod
      hpkgs.hasktags
      hpkgs.haskdogs  # stack config set system-ghc --global true
      hpkgs.hdevtools
      hpkgs.hlint
      hpkgs.pointfree
      hpkgs.pointful
      /* hpkgs.stack */
    ];
  };

  adjust =
    if compiler == "ghcjs"
    then adjust-for-ghcjs
    else adjust-for-ghc;

  haskellPackages = reflex-platform.${compiler}.override {
    overrides = (self: super: {
      ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
      ghcWithPackages = self.ghc.withPackages;
      wai-middleware-static = 
        pkgs.haskell.lib.dontCheck (super.wai-middleware-static);
      reflex-dom-htmlea = hLib.dontHaddock 
        (self.callCabal2nix "reflex-dom-htmlea" (
          initialNixpkgs.pkgs.fetchFromGitHub { 
            owner  = "gspia";
            repo   = "reflex-dom-htmlea";
            rev = "49bb339a6142188482c88a8ab55dfc71206ed9f6";
            sha256 = "1qk7szm923whnalbqy9gwq4wcazz1lyh4r1iz4b86ab6kw1hrk98";
          }
      ) {} );
    });
  };

  themes-code-base = 
    haskellPackages.callPackage ./reflex-dom-themes.nix { inherit compiler; };
  themes-code = 
    pkgs.haskell.lib.overrideCabal themes-code-base adjust;
in
  themes-code
