{ ghc ? "ghc922" }:

let
  nixpkgs = import ./nixpkgs.nix { };

  overlay = self: super: {
    haskell = super.haskell // {
      packages = super.haskell.packages // {
        "${ghc}" = super.haskell.packages."${ghc}".override (_: {
          overrides = self.lib.composeManyExtensions [
            (new: _: {
              bitmaps     = new.callCabal2nix "bitmaps" ../. { };
              prim-bool   = new.callPackage pkgs/prim-bool.nix { };
              prim-compat = new.callPackage pkgs/prim-compat.nix { };
              prim-int    = new.callPackage pkgs/prim-int.nix { };
            })
          ];
        });
      };
    };
  };

in import nixpkgs {
  overlays = [ overlay ];
}