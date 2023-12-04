// to assemble one jar file including all dependencies
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.10")

// to generate a javascript file and run the compiler in the browser
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.14.0")

// to have separate projects for jvm and js details
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.2")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "1.3.2")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.14")

// build native images using GraalVM
// addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.1.2")
