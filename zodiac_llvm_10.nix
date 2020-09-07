{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    name = "zodiac_llvm10";
    buildInputs = [
      pkgs.cmake
      pkgs.ninja
      pkgs.clang
      pkgs.gdb
      pkgs.llvm_10

      pkgs.zlib
      pkgs.ncurses
    ];
}

