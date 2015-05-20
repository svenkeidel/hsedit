{ pkgs ? import <nixpkgs> {} }:

with pkgs.haskellPackages; cabal.mkDerivation (self: {
  pname = "distributed-editor";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    cabalInstall attoparsec haskellSrcMeta
  ];
  meta = {
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
