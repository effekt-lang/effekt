import sbtcrossproject.CrossProject

enablePlugins(ScalaJSPlugin)

lazy val replDependencies = Seq(
  "jline" % "jline" % "2.14.6",
  "org.rogach" %% "scallop" % "4.1.0",
)

lazy val lspDependencies = Seq(
  "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.23.1",
  "com.google.code.gson" % "gson" % "2.11.0"
)

lazy val testingDependencies = Seq(
  "org.scala-sbt" %% "io" % "1.6.0" % Test,
  "org.scalameta" %% "munit" % "0.7.29" % Test
)

lazy val kiama: CrossProject = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(Seq(
    scalaVersion := "3.3.1",
    scalacOptions ++= Seq(
      "-encoding", "utf8",
      "-deprecation",
      "-unchecked",
      "-Xfatal-warnings",
      "-feature",
      "-language:existentials",
      "-language:higherKinds",
      "-language:implicitConversions"
    ),
    publish := {},
    publishLocal := {}
  ))
  .settings(
    name := "kiama"
  )
  .jvmSettings(
    libraryDependencies ++= (replDependencies ++ lspDependencies ++ testingDependencies),

    Test / testOptions += Tests.Argument(TestFrameworks.MUnit, "-oD"),
  )
