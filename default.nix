{
  pkgs ? import <nixpkgs> {}
}:

with pkgs;
stdenv.mkDerivation {
  name = "elm";
  buildInputs = [ elmPackages.elm
                  elmPackages.elm-language-server
                ];
}