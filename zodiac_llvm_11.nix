{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    name = "zodiac_llvm11";
    buildInputs = [
      pkgs.llvm_11
    ];
}
