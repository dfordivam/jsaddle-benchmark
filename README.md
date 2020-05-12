WIP: BMs to get jsaddle (warp, webgtk, etc) performance

With current jsaddle 0.9.7.0
----------------------------

|Description|time (in sec)|
|--- |--- |
|valToBool|0.000172329s|
|valToNumber|0.254734766s|
|valToStr|0.295951065s|
|valToText|0.315967137s|
|valToObject|0.00006028s|
|valToJSON|0.292323465s|
|toJSVal Bool|0.000048194s|
|toJSVal Double|0.443135987s|
|toJSVal String|0.526170742s|
|toJSVal Text|0.357108685s|
|makeObject|0.000747539s|
|makeObject + getProp|0.002625864s|
|makeObject + valToJSON|0.355244738s|
|array|0.001499996s|
|array + propertyNames|0.819106474s|
|array + properties|0.853256681s|
|getProp bool|0.659680151s|
|getProp number|0.450440087s|
|getProp text|0.701161348s|
|setProp bool|0.000164276s|
|setProp number|0.000471348s|
|setProp text|0.000773491s|
|getProp + setProp bool|0.395728894s|
|getProp + setProp number|0.369143312s|
|getProp + setProp text|0.416171591s|
|call string length|0.449031207s|


With experimental-core2-master-2 branch
---------------------------------------

|Description|time (in sec)|
|--- |--- |
|valToBool|0.000163689s|
|valToNumber|0.000118897s|
|valToStr|0.00018741s|
|valToText|0.00011187s|
|valToObject|0.00011307s|
|valToJSON|0.535083819s|
|toJSVal Bool|0.000277155s|
|toJSVal Double|0.000221381s|
|toJSVal String|0.000180695s|
|toJSVal Text|0.001097639s|
|makeObject|0.603425095s|
|makeObject + getProp|0.655101603s|
|makeObject + valToJSON|1.046433399s|
|array|0.380736652s|
|array + propertyNames|1.930186728s|
|array + properties|2.834575515s|
|getProp bool|0.547005219s|
|getProp number|0.382025153s|
|getProp text|0.320978005s|
|setProp bool|0.077073424s|
|setProp number|0.064508024s|
|setProp text|0.070279316s|
|getProp + setProp bool|0.340852815s|
|getProp + setProp number|0.542273733s|
|getProp + setProp text|0.475722744s|
|call string length|0.354996312s|

