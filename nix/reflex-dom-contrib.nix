let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    reflex-dom-contrib-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./reflex-dom-contrib.json;
    reflex-dom-contrib = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "reflex-frp";
      repo = "reflex-dom-contrib";
      inherit (reflex-dom-contrib-info-pinned) rev sha256;
    };
  };
in
  sources.reflex-dom-contrib

