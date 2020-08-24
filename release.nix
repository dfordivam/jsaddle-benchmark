let
  nixpkgs = (import ./dep/reflex-platform-master {}).nixpkgs;
  inherit (nixpkgs.haskell.lib) dontCheck appendPatch overrideCabal;

  master-project = (import ./reflex-project.nix {
    appNameSuffix = "_master";
  });

  mkProject = { jsaddleSrc, appNameSuffix, reflexDomPatches }: import ./reflex-project.nix {
    inherit appNameSuffix;
    otherOverrides = (self: super: {
      jsaddle = self.callCabal2nix "jsaddle" (jsaddleSrc + "/jsaddle") {};
      jsaddle-warp = if super.jsaddle-warp == null
        then null
        else dontCheck (self.callCabal2nix "jsaddle-warp" (jsaddleSrc + "/jsaddle-warp") {});
      jsaddle-webkit2gtk = if super.jsaddle-webkit2gtk == null
        then null
        else self.callCabal2nix "jsaddle-webkit2gtk" (jsaddleSrc + "/jsaddle-webkit2gtk") {};
      jsaddle-dom = dontCheck (appendPatch super.jsaddle-dom ./dep/jsaddle-dom-fix.patch);
      reflex-dom-core = dontCheck (super.reflex-dom-core);
      reflex-dom = dontCheck (overrideCabal super.reflex-dom (drv: {
        patches = (drv.patches or []) ++ reflexDomPatches;
      }));
    });
  };

  exp-core-project = mkProject {
    jsaddleSrc = import dep/jsaddle-exp-core2/thunk.nix;
    appNameSuffix = "_core2";
    reflexDomPatches = [
      dep/reflex-dom-fixes-for-experimental-core2-branch.patch
    ];
  };

  exp-core-req-batch-project = mkProject {
    jsaddleSrc = import dep/jsaddle-exp-core2-req-batching/thunk.nix;
    appNameSuffix = "_core2_req_batch";
    reflexDomPatches = [
      dep/reflex-dom-fixes-for-experimental-core2-branch.patch
      dep/reflex-dom-fix-for-batching-of-requests.patch
    ];
  };

  exp-core-req-resp-batch-project = mkProject {
    jsaddleSrc = import dep/jsaddle-exp-core2-req-resp-batching/thunk.nix;
    appNameSuffix = "_core2_req_resp_batch";
    reflexDomPatches = [
      dep/reflex-dom-fixes-for-experimental-core2-branch.patch
      dep/reflex-dom-fix-for-batching-of-requests.patch
      dep/reflex-dom-fix-for-batching-of-responses.patch
    ];
  };

in {
  inherit master-project exp-core-project exp-core-req-batch-project;

  # master is reflex-platform's default jsaddle
  master-android = master-project.android.jsaddle-benchmark;
  master-ghc = master-project.ghc.jsaddle-benchmark;
  master-ghcjs = master-project.ghcjs.jsaddle-benchmark;

  # Experimental Core 2
  exp-core-android = exp-core-project.android.jsaddle-benchmark;
  exp-core-ghc     = exp-core-project.ghc.jsaddle-benchmark;

  # Experimental Core 2 with request batching
  exp-core-req-batch-android = exp-core-req-batch-project.android.jsaddle-benchmark;
  exp-core-req-batch-ghc     = exp-core-req-batch-project.ghc.jsaddle-benchmark;

  # Experimental Core 2 with request + response batching (batch size - 10 for jsaddle-wasm, 20 for android)
  exp-core-req-resp-batch-android = exp-core-req-resp-batch-project.android.jsaddle-benchmark;
  exp-core-req-resp-batch-ghc     = exp-core-req-resp-batch-project.ghc.jsaddle-benchmark;
}
