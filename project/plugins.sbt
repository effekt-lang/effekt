// to assemble one jar file including all dependencies
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "2.4.1")

// to generate a javascript file and run the compiler in the browser
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.22.0")

// to have separate projects for jvm and js details
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.4.0")

// build native images using GraalVM
// addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.1.2")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.6.1")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.7")

// to update dependency versions
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.7.0")
