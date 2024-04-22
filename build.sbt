import sbtcrossproject.CrossProject

lazy val effektVersion = "0.2.2"

lazy val commonSettings = Seq(
  scalaVersion := "3.3.1",
  semanticdbEnabled := true,
  scalacOptions ++= Seq(
    "-encoding", "utf8",
    "-deprecation",
    "-unchecked",
    // "-Xlint",
    // "-Xcheck-macros",
    "-Xfatal-warnings",
    // we can use scalafix's organize imports once the next Scala version is out.
    // https://github.com/scalacenter/scalafix/pull/1800
    // "-Wunused:imports",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions"
  )
)

enablePlugins(ScalaJSPlugin)


lazy val root = project.in(file("effekt") / "shared")
  .settings(
    name := "effekt",
    version := effektVersion
  )
  .settings(commonSettings)


