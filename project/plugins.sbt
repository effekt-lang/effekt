// to assemble one jar file including all dependencies
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.10")

// to generate a javascript file and run the compiler in the browser
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.0.1")

// to have separate projects for jvm and js details
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.0.0")

// to automatically format the Scala code according to some guidelines
addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.3")
