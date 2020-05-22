{ nixpkgs ?
  # The current nixpkgs used in reflex-platform develop
  import (builtins.fetchTarball {
    url = https://github.com/WebGHC/nixpkgs/archive/423fa4adb6276298cde0c12205efeb7bf1cbe805.tar.gz;
    sha256 = "0ssvc5j0p7nip0qsrdbfgc6wihzwyxa23hbr80dmjbfizywz7nzg";
  }) {}
}:

let
inherit (nixpkgs) pkgs fetchFromGitHub;

  # 0.9.7.0
  jsaddle-master =  fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "4b135448f425edf968d5058b901b57064c9d2b7a";
    sha256 = "0xpsv1pp1a13vq5vk1wjj1iq0cfnq9cv7lkrv2rl6yd47slwmn2a";
  };
  jsaddle-exp-core =  fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "jsaddle";
    rev = "af448aaf4c005e949196c5fb314dc4bdf4fe8519";
    sha256 = "1ypsfwnz5nxmisxyqvrg681339kq60kl7g4y74dah3vfnmrg3dlw";
  };

  using-jsaddle = jsaddle-src: with pkgs.haskell.packages.ghc865; extend (pkgs.lib.composeExtensions (packageSourceOverrides {
      jsaddle = jsaddle-src + /jsaddle;
      jsaddle-warp = jsaddle-src + /jsaddle-warp;
    })
    (self: super: {
      jsaddle-warp = pkgs.haskell.lib.dontCheck super.jsaddle-warp;
    })
  );

  mkDrv = haskellPkgs: haskellPkgs.callCabal2nix "jsaddle-benchmark" ./. { };
in {
  master = let drv = mkDrv (using-jsaddle jsaddle-master); in if pkgs.lib.inNixShell then drv.env else drv;
  exp-core = let drv = mkDrv (using-jsaddle jsaddle-exp-core); in if pkgs.lib.inNixShell then drv.env else drv;
}