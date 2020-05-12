WIP: BMs to get jsaddle (warp, webgtk, etc) performance

With current jsaddle 0.9.7.0
----------------------------
|	Description	|	master	|	Experimental-core2	|
|	---	|	---	|	---	|
|	valToBool	|	0.000172329s	|	0.000163689s	|
|	valToNumber	|	0.254734766s	|	0.000118897s	|
|	valToStr	|	0.295951065s	|	0.00018741s	|
|	valToText	|	0.315967137s	|	0.00011187s	|
|	valToObject	|	0.00006028s	|	0.00011307s	|
|	valToJSON	|	0.292323465s	|	0.535083819s	|
|	toJSVal Bool	|	0.000048194s	|	0.000277155s	|
|	toJSVal Double	|	0.443135987s	|	0.000221381s	|
|	toJSVal String	|	0.526170742s	|	0.000180695s	|
|	toJSVal Text	|	0.357108685s	|	0.001097639s	|
|	makeObject	|	0.000747539s	|	0.603425095s	|
|	makeObject + getProp	|	0.002625864s	|	0.655101603s	|
|	makeObject + valToJSON	|	0.355244738s	|	1.046433399s	|
|	array	|	0.001499996s	|	0.380736652s	|
|	array + propertyNames	|	0.819106474s	|	1.930186728s	|
|	array + properties	|	0.853256681s	|	2.834575515s	|
|	getProp bool	|	0.659680151s	|	0.547005219s	|
|	getProp number	|	0.450440087s	|	0.382025153s	|
|	getProp text	|	0.701161348s	|	0.320978005s	|
|	setProp bool	|	0.000164276s	|	0.077073424s	|
|	setProp number	|	0.000471348s	|	0.064508024s	|
|	setProp text	|	0.000773491s	|	0.070279316s	|
|	getProp + setProp bool	|	0.395728894s	|	0.340852815s	|
|	getProp + setProp number	|	0.369143312s	|	0.542273733s	|
|	getProp + setProp text	|	0.416171591s	|	0.475722744s	|
|	call string length	|	0.449031207s	|	0.354996312s	|
