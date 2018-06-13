{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};

  modifiedHaskellPackages = ghc.override {
    overrides = self: super: {
      reflex-dom-contrib = 
        super.callCabal2nix "reflex-dom-contrib" (import ./nix/reflex-dom-contrib.nix) {};
      reflex-dom-nested-routing = 
        super.callCabal2nix "reflex-dom-nested-routing" (import ./nix/reflex-dom-nested-routing.nix) {};
    };
  };

  drv = modifiedHaskellPackages.callPackage ./reflex-dom-storage.nix {};
in
  drv
