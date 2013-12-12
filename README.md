scalabc
=======

scalabc offers an API for approximate bayesian computation (ABC) sampling schemes
within your model. In order to perform your ABC analysis, you have to provide your
model and choose the algorithm to use.

Implemented algorithm:
 * Beaumont: Beaumont, M. A., Cornuet, J., Marin, J., and Robert, C. P. (2009) Adaptive approximate Bayesian computation.
*Biometrika*, 96, pp 983–990
 * Lenormand: Lenormand M., Jabot F., Deffuant G. (2013). Adaptive approximate Bayesian computation for complex models.
*Computational Statistics* Volume 28, Issue 6, pp 2777-2796
 
You can tune these algorithms by providing your own distance function and your own particle moving function.

## Examples

You can find some examples in the object [Tests](src/main/scala/fr/irstea/easyabc/Test.scala)
