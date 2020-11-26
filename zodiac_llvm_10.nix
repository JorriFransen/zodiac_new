{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    name = "zodiac_llvm10";
    buildInputs = [
      pkgs.llvm_10
    ];
}
