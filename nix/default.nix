let
  pkgs = import ./nixpkgs.nix { overlays = import ./overlays.nix; };
in
with pkgs;

{ inherit (haskellPackages) repline;
  inherit repline-shell;
}
