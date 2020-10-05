{
  pkgs ? import <nixpkgs> {}
}:

with pkgs;
stdenv.mkDerivation {
  name = "pandoc";
  buildInputs = [ pkgs.pandoc
                  (texlive.combine {
                    inherit (texlive)
                      latexmk
                      scheme-small;
                  })
                ];
}