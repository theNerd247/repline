let 
  shared = (import ./nix);
in 
with shared.pkgs;

shared.repline-shell.overrideAttrs (old: 
  { buildInputs = old.buildInputs ++ [ cowsay ];
  }
)
