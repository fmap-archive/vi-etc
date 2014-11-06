let 
  nixpkgs = (import <nixpkgs> {}).fetchgit {
    url    = "https://github.com/nixos/nixpkgs.git";
    rev    = "782a12e450a58b815cd702c65da75855124d1572";
    sha256 = "e73e534a6a0c671510e495016f8054fef14c7c35479406e3ee8e4e6a35a8954e";
  };
in
{ pkgs ? import nixpkgs {}
, haskellPackages ? pkgs.haskellPackages_ghc783
}:
let
  inherit (pkgs) ghc stdenv;
  inherit (haskellPackages) xmonad xmonadContrib xmonadExtras interpolate;
in stdenv.mkDerivation rec {
  name = "vi-etc";
  buildInputs = [ghc.ghc783 xmonad xmonadContrib xmonadExtras interpolate];
}
