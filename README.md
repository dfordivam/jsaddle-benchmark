WIP: BMs to get jsaddle (warp, webgtk, etc) performance
------------------------------

Hack / Run
----------

```
nix-shell -A master;
cabal configure; cabal run
```
```
$(nix-build shell.nix -A master)/bin/jsaddle-benchmark
```
```
nix-shell -A exp-core;
cabal configure; cabal run
```
```
$(nix-build shell.nix -A exp-core)/bin/jsaddle-benchmark
```

Optionally the port, count and benchmark prefix can be specified

```
$(nix-build shell.nix -A exp-core)/bin/jsaddle-benchmark 8000 1 "makeObject only"
```

With chromium performance comparison
----------------------------

| Description                     | master       | Experimental-core2 |
| ---                             | ---          | ---                |
| valToBool                       | 0.000492867  | 0.0001591095       |
| valToNumber                     | 0.4076312145 | 0.0002671375       |
| valToStr                        | 0.2822356795 | 0.000111187        |
| valToText                       | 0.3766808455 | 0.000111314        |
| valToObject                     | 0.0000804515 | 0.000088367        |
| valToJSON                       | 0.4200538965 | 0.312987126        |
| toJSVal Bool                    | 0.0000808415 | 0.0000851945       |
| toJSVal Double                  | 0.4147958135 | 0.0000601615       |
| toJSVal String                  | 0.4095505025 | 0.0001001135       |
| toJSVal Text                    | 0.3516210095 | 0.0001418955       |
| makeObject only                 | 0.001022331  | 0.221493649        |
| makeObject + getProp            | 0.0020759235 | 0.4196346015       |
| makeObject + valToJSON          | 0.4190759595 | 0.4633202545       |
| array only                      | 0.002131875  | 0.2519300715       |
| array + propertyNames           | 0.7508036225 | 0.716924548        |
| array + properties              | 0.8674880525 | 1.1763473065       |
| getProp bool                    | 0.3444904205 | 0.2977317685       |
| getProp number                  | 0.330152167  | 0.240401359        |
| getProp text                    | 0.3739460315 | 0.242306344        |
| setProp bool                    | 0.000346412  | 0.0340490785       |
| setProp number                  | 0.00245655   | 0.0379504365       |
| setProp text                    | 0.0004806915 | 0.042388585        |
| getProp + setProp bool          | 0.0022888915 | 0.1260138845       |
| getProp + setProp number        | 0.0148245195 | 0.13312601         |
| getProp + setProp text          | 0.0018280615 | 0.113970165        |
| getProp, modify, setProp bool   | 0.529147232  | 0.3493258985       |
| getProp, modify, setProp number | 0.3745327705 | 0.382486941        |
| getProp, modify, setProp text   | 0.348473766  | 0.3312219025       |
| strictEqual bool                | 0.2685219165 | 0.4161083485       |
| strictEqual number              | 0.2491838255 | 0.523061596        |
| strictEqual string              | 0.267813703  | 0.4247112975       |
| strictEqual object              | 0.4183863065 | 0.5098007085       |
| call string length              | 0.3593309895 | 0.236497824        |



With Firefox performance comparison
----------------------------

| Description                     | master       | Experimental-core2 |
| ---                             | ---          | ---                |
| valToBool                       | 0.000264517  | 0.000300147        |
| valToNumber                     | 0.516842034  | 0.0001956975       |
| valToStr                        | 0.3096834175 | 0.0001269695       |
| valToText                       | 0.41148966   | 0.0001518725       |
| valToObject                     | 0.00005957   | 0.0001229175       |
| valToJSON                       | 0.323982182  | 0.379617305        |
| toJSVal Bool                    | 0.0000655995 | 0.0001237805       |
| toJSVal Double                  | 0.482404246  | 0.0001276845       |
| toJSVal String                  | 0.544250052  | 0.000136677        |
| toJSVal Text                    | 0.4539594765 | 0.000062933        |
| makeObject only                 | 0.001686424  | 0.1460690015       |
| makeObject + getProp            | 0.0070407825 | 0.205820224        |
| makeObject + valToJSON          | 0.483572392  | 0.5984359445       |
| array only                      | 0.0031455235 | 0.1672154045       |
| array + propertyNames           | 1.2468368265 | 1.148511717        |
| array + properties              | 0.942792931  | 1.8808531275       |
| getProp bool                    | 0.637728977  | 0.379094421        |
| getProp number                  | 0.53671459   | 0.3983319215       |
| getProp text                    | 0.4947539285 | 0.346531853        |
| setProp bool                    | 0.0003189605 | 0.044762291        |
| setProp number                  | 0.000993333  | 0.04902562         |
| setProp text                    | 0.003960731  | 0.04759634         |
| getProp + setProp bool          | 0.0012221705 | 0.264157855        |
| getProp + setProp number        | 0.0013545225 | 0.1935074515       |
| getProp + setProp text          | 0.0015044245 | 0.2082026335       |
| getProp, modify, setProp bool   | 0.641798559  | 0.4867152775       |
| getProp, modify, setProp number | 0.681473626  | 0.44508916         |
| getProp, modify, setProp text   | 0.4639605955 | 0.389791841        |
| strictEqual bool                | 0.3171709985 | 0.8729372035       |
| strictEqual number              | 0.3966068895 | 0.5622770625       |
| strictEqual string              | 0.4819113915 | 0.6148168215       |
| strictEqual object              | 0.43574259   | 0.763746554        |
| call string length              | 0.3943247745 | 0.3763913815       |
