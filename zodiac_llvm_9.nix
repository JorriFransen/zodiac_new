{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    name = "zodiac_llvm9";
    buildInputs = [
      pkgs.cmake
      pkgs.ninja
      pkgs.clang
      pkgs.gdb
      pkgs.llvm_9

      pkgs.zlib
      pkgs.ncurses
    ];
}

