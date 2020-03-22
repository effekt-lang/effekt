val dottyVersion = "2.13.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "effekt",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies ++= dependencies, //.map(d => d.withDottyCompat(scalaVersion.value)),

    mainClass in assembly := Some("effekt.Server"),
    assemblyJarName in assembly := "effekt.jar",
    Test / parallelExecution := false
  )

lazy val dependencies = Seq(
  "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0",
  "org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0",
  "com.novocode" % "junit-interface" % "0.11" % "test",
  "org.scala-sbt" %% "io" % "1.3.1" % "test"
)