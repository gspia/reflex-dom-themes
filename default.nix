{}:
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    reflex-dom-themes  = ./.;
    examplesBulma  = ./examplesBulma;
    examplesW3  = ./examplesW3;
    examplesBs  = ./examplesBs;
    examplesFou = ./examplesFou;
    examplesSem = ./examplesSem;
  };
  overrides = self: super: {
    reflex-dom-htmlea = self.callCabal2nix "reflex-dom-htmlea"
      # ../reflex-dom-htmlea {};  /* use this when doing things locally */
      (pkgs.fetchFromGitHub {
        owner = "gspia";
        repo = "reflex-dom-htmlea";
        rev = "0baf429ad8e4a19b6e2575ffc828efe521e44f0f";
        sha256 = "1aly8n6p746cgbabsp54ynfjsfbcj64np04rzma46djvwq4kcfv0";
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
              "examplesSem" "examplesW3" "examplesBulma" ];
    ghcjs = [ "reflex-dom-themes" "examplesBs" "examplesFou"
              "examplesSem" "examplesW3" "examplesBulma" ];
  };
  tools = ghc: with ghc; [
    pkgs.haskellPackages.hlint
    /* pkgs.haskellPackages.hdevtools */
    # pkgs.haskellPackages.hasktags
    # pkgs.haskellPackages.haskdogs
    # pkgs.haskellPackages.hindent
    # pkgs.haskellPackages.hsimport
    # pkgs.haskellPackages.pointfree
    # pkgs.haskellPackages.pointful
    # pkgs.haskellPackages.stylish-haskell
  ];
})
