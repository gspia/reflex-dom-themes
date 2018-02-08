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
      # ../reflex-dom-htmlea {};  /* use this when doing things locally */
      (pkgs.fetchFromGitHub {
        owner = "gspia";
        repo = "reflex-dom-htmlea";
        rev = "64a3ad9c234a722f76841839a9a7c5caa68318dc";
        sha256 = "0y53la83n969b3py2smckrrn1g45i9hwbz7jn77aisfydpd0blda";
      }) {};
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
    /* pkgs.haskellPackages.ghc-mod */
    pkgs.haskellPackages.hasktags
    pkgs.haskellPackages.haskdogs
    /* pkgs.haskellPackages.hdevtools */
    pkgs.haskellPackages.hindent
    pkgs.haskellPackages.hsimport
    pkgs.haskellPackages.hlint
    pkgs.haskellPackages.pointfree
    pkgs.haskellPackages.pointful
    pkgs.haskellPackages.stylish-haskell
  ];
})
