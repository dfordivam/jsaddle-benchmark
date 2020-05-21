{
  exp-core-android = (import ./reflex-project.nix {
      appNameSuffix = "_exp_core";
    }).android.jsaddle-benchmark;
  master-android = (import ./reflex-project.nix {
      reflex-platform = import ./reflex-platform-master;
      appNameSuffix = "_master";
    }).android.jsaddle-benchmark;
}