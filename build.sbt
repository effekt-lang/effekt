val dottyVersion = "0.21.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "effekt",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= Seq(
        ("org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0").withDottyCompat(scalaVersion.value),
        ("org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0").withDottyCompat(scalaVersion.value),
        ("com.novocode" % "junit-interface" % "0.11" % "test").withDottyCompat(scalaVersion.value),
        ("org.scala-sbt" %% "io" % "1.3.1" % "test").withDottyCompat(scalaVersion.value)
    ),

    mainClass in assembly := Some("effekt.Server"),
    assemblyJarName in assembly := "effekt.jar"
  )
