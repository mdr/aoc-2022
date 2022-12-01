{ pkgs ? (import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/ce5fe99df1f15a09a91a86be9738d68fadfbad82.tar.gz") { }) }:
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    idris2
    chez-racket
    rlwrap
  ];
}
