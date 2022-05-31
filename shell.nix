{ pkgs ? import <nixpkgs> {}}: with pkgs;

mkShell {
  buildInputs = [
    elmPackages.elm
  ];
}
