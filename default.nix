{
  pkgs ? import <nixpkgs> {}
}:

with pkgs;
stdenv.mkDerivation {
  name = "elm";
  buildInputs = [ elmPackages.elm ];
}