{
  system ? builtins.currentSystem,
  compilerVersion ? "ghc8104",
  nixpkgs ? import ./nix/pinned-nixpkgs.nix,
  doCheck ? false,
  doHaddock ? false,
  doHoogle ? true
}:

let
  config = {
    packageOverrides = super:
    let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packages = super.haskell.packages // {
          "${compilerVersion}" = super.haskell.packages.${compilerVersion}.override {
            overrides = hself: hsuper: rec {

              Multiformats =
                super.haskell.lib.overrideCabal
                ( hself.callCabal2nix "Multiformats" ./. {})
                (drv: {
                  inherit doHaddock doHoogle doCheck;
                });
            };
          };
        };
      };
    };
  };
  pkgs = import nixpkgs { inherit config; inherit system; };
in
  {
    inherit pkgs;
    hsPkgs = pkgs.haskell.packages.${compilerVersion};
  }
