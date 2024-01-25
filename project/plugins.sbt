// to assemble one jar file including all dependencies
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.10")

// to generate a javascript file and run the compiler in the browser
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.14.0")

// to have separate projects for jvm and js details
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")

// build native images using GraalVM
// addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.1.2")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")

addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.11.1")

// for the lsp server tests
addSbtPlugin("org.scalablytyped.converter" % "sbt-converter" % "1.0.0-beta43")