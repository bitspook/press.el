with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "bitspook-in";
  buildInputs = with pkgs; [
    yarn
  ];
}
