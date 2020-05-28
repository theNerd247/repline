self: super:

let
  sourceOverrides = self.haskell.lib.packageSourceOverrides
    { repline = ../.;
      haskeline = "0.8.0.0";
    };
in

{ haskellPackages = super.haskellPackages.override 
  (old:
    { overrides = super.lib.composeExtensions sourceOverrides 
      (hself: hsuper: with super.haskell.lib;
        { repline = doJailbreak hsuper.repline;
          haskeline = dontCheck hsuper.haskeline;
        }
      );
    }
  );
}
