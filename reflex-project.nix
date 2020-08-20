{ reflex-platform ? import ./reflex-platform,
  appNameSuffix ? ""
} :
(reflex-platform { config.android_sdk.accept_license = true;}).project ({ pkgs, ... }: {
  packages = {
    jsaddle-benchmark = ./.;
  };

  shells = {
    ghc = ["jsaddle-benchmark"];
    ghcjs = ["jsaddle-benchmark"];
  };

  overrides = self: super: {
    jsaddle-benchmark = with pkgs.haskell.lib;
      let
        bm = disableCabalFlag super.jsaddle-benchmark "disable-reflex";
      in overrideCabal bm (drv: {
        buildDepends = (drv.buildDepends or []) ++ [
          self.reflex-dom self.safe
        ];
      });
  };

  android.jsaddle-benchmark = {
    executableName = "jsaddle-benchmark-reflex";
    applicationId = "org.reflex_frp.jsaddle_benchmark" + appNameSuffix;
    displayName = "JSaddle BM" + appNameSuffix;
  };
})