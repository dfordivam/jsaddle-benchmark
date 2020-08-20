let
  exp-core-reflex-project = (import ./reflex-project.nix {
      reflex-platform = import ./reflex-platform;
      appNameSuffix = "_exp_core";
  });
  master-reflex-project = (import ./reflex-project.nix {
      reflex-platform = import ./reflex-platform-master;
      appNameSuffix = "_master";
  });
in {
  exp-core = (import ./shell.nix {}).exp-core;
  master = (import ./shell.nix {}).master;
  exp-core-android = exp-core-reflex-project.android.jsaddle-benchmark;
  exp-core-ghc = exp-core-reflex-project.ghc.jsaddle-benchmark;
  exp-core-ghcjs = exp-core-reflex-project.ghcjs.jsaddle-benchmark;
  master-android = master-reflex-project.android.jsaddle-benchmark;
  master-ghc = master-reflex-project.ghc.jsaddle-benchmark;
  master-ghcjs = master-reflex-project.ghcjs.jsaddle-benchmark;
}
