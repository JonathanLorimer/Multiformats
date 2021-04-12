let
  hostNix = import <nixpkgs> {};
  nixpkgsPin = hostNix.pkgs.lib.importJSON ./nixpkgs-version.json;
	pinnedPkgs = hostNix.pkgs.fetchFromGitHub {
			owner = "NixOS";
			repo  = "nixpkgs-channels";
			inherit (nixpkgsPin) rev sha256;
  };
in
  pinnedPkgs
