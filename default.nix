{}:
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    reflex-dom-themes  = ./.;
    examplesBs  = ./examplesBs;
    examplesFou = ./examplesFou;
    examplesSem = ./examplesSem;
    examplesW3  = ./examplesW3;
  };
  overrides = self: super: {
    reflex-dom-htmlea = self.callCabal2nix "reflex-dom-htmlea"
      (pkgs.fetchFromGitHub {
        owner = "gspia";
        repo = "reflex-dom-htmlea";
        rev = "0e682b302d748ae68e93aad9b39817c789220485";
        sha256 = "0h5lz6bnacdr46a34k6cxa2pfrdcv7kvga4kws280hwi9z7qmvgb";
      }) {};
    /* lens = self.callHackage "lens" "4.15.4" {}; */
    /* free = self.callCabal2nix "free" (pkgs.fetchFromGitHub { */
    /*   owner = "ekmett"; */
    /*   repo = "free"; */
    /*   rev = "a0c5bef18b9609377f20ac6a153a20b7b94578c9"; */
    /*   sha256 = "0vh3hj5rj98d448l647jc6b6q1km4nd4k01s9rajgkc2igigfp6s"; */
    /* }) {}; */
  };

  /* android.exampleTbl = { */
  /*   executableName = "exampleTbl"; */
  /*   applicationId = "org.example.exampleTbl"; */
  /*   displayName = "Example Tables App"; */
  /* }; */
  /* ios.keyboard = { */
  /*   executableName = "keyboard"; */
  /*   bundleIdentifier = "org.example.keyboard"; */
  /*   bundleName = "Example iOS App (keyboard ex)"; */
  /* }; */

  shells = {
    ghc   = [ "reflex-dom-themes" "examplesBs" "examplesFou" 
              "examplesSem" "examplesW3" ];
    ghcjs = [ "reflex-dom-themes" "examplesBs" "examplesFou"
              "examplesSem" "examplesW3" ];
  };
  tools = ghc: with ghc; [
    pkgs.haskellPackages.ghc-mod
    pkgs.haskellPackages.hasktags
    pkgs.haskellPackages.haskdogs
    pkgs.haskellPackages.hdevtools
    pkgs.haskellPackages.hindent
    pkgs.haskellPackages.hsimport
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.pointfree
    pkgs.haskellPackages.pointful
    pkgs.haskellPackages.stylish-haskell
  ];
})
