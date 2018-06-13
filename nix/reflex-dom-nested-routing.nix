let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    reflex-dom-nested-routing-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./reflex-dom-nested-routing.json;
    reflex-dom-nested-routing = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "3noch";
      repo = "reflex-dom-nested-routing";
      inherit (reflex-dom-nested-routing-info-pinned) rev sha256;
    };
  };
in
  sources.reflex-dom-nested-routing

