This project is obsolete. Longs have made it into Scala.js for a long time and have much (!) improved since. Please look at [RuntimeLong][1] in the [Scala.js][2] project.

This project aims to implement 64-bit longs for scala-js in order to
remove semantic differences.

The final goal is to integrate them in the compiler, much like the
automatic replacement of `java.lang.String` 

[1]: https://github.com/scala-js/scala-js/blob/master/library/src/main/scala/scala/scalajs/runtime/RuntimeLong.scala
[2]: https://github.com/scala-js/scala-js
