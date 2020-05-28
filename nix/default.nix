let
  pkgs = import ./nixpkgs.nix { overlays = import ./overlays.nix; };
in

{ inherit pkgs;
  inherit (pkgs.haskellPackages) repline;
  inherit (pkgs) repline-shell;
}
