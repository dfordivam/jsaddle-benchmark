{ reflex-platform-func ? import ./dep/reflex-platform-master,
  appNameSuffix ? "",
  otherOverrides ? (_: _: {})
} :
let
  reflex-platform = reflex-platform-func {
    config.android_sdk.accept_license = true;
  };
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    jsaddle-benchmark = pkgs.lib.cleanSource ./.;
  };

  shells = {
    ghc = ["jsaddle-benchmark"];
    ghcjs = ["jsaddle-benchmark"];
  };

  overrides = reflex-platform.nixpkgs.lib.composeExtensions otherOverrides (self: super: {
    jsaddle-benchmark = with pkgs.haskell.lib;
      let
        bm = disableCabalFlag super.jsaddle-benchmark "disable-reflex";
      in overrideCabal bm (drv: {
        buildDepends = (drv.buildDepends or []) ++ [
          self.reflex-dom self.safe
        ];
      });
  });

  android.jsaddle-benchmark = {
    executableName = "jsaddle-benchmark-reflex";
    applicationId = "org.reflex_frp.jsaddle_benchmark" + appNameSuffix;
    displayName = "JSaddle BM" + appNameSuffix;
  };
})