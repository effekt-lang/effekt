enablePlugins(ScalaJSPlugin)

lazy val root = project.in(file(".")).
  aggregate(effekt.js, effekt.jvm).
  settings(
    publish := {},
    publishLocal := {},
  )

lazy val effekt = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "effekt",
    version := "0.1.1",
    scalaVersion := "2.13.1"
  ).
  jvmSettings(
    mainClass in assembly := Some("effekt.Server"),
    assemblyJarName in assembly := "effekt.jar",
    Test / parallelExecution := false,

    libraryDependencies ++= Seq(
      "org.rogach" %% "scallop" % "3.4.0",
      "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0",
      "org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0",
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.scala-sbt" %% "io" % "1.3.1" % "test"
    )
  ).
  jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq(
      "org.bitbucket.inkytonik.kiama" %%% "kiama" % "2.4.0-SNAPSHOT"
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
//    scalaJSUseMainModuleInitializer := true
  )