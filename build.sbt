import sbtcrossproject.CrossProject
import scalariform.formatter.preferences.AlignSingleLineCaseStatements.MaxArrowIndent
import scalariform.formatter.preferences._

enablePlugins(ScalaJSPlugin)

lazy val effektVersion = "0.1.4"

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
)

lazy val commonSettings = Seq(
  scalaVersion := "2.13.1",
  scalacOptions ++= Seq(
    "-encoding", "utf8",
    "-deprecation",
    "-unchecked",
    // "-Xlint",
    "-Xfatal-warnings",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
  ),
  scalariformPreferences := scalariformPreferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(DanglingCloseParenthesis, Force)
    .setPreference(NewlineAtEndOfFile, true)
    .setPreference(MaxArrowIndent, 20)
)


lazy val root = project.in(file("."))
  .aggregate(effekt.js, effekt.jvm)
  .settings(noPublishSettings)

lazy val effekt: CrossProject = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "effekt",
    version := effektVersion
  )
  .settings(commonSettings)
  .jvmSettings(
    mainClass in assembly := Some("effekt.Server"),
    assemblyJarName in assembly := "effekt.jar",
    Test / parallelExecution := false,

    libraryDependencies ++= Seq(
      "org.rogach" %% "scallop" % "3.4.0",
      "org.bitbucket.inkytonik.kiama" %% "kiama" % "2.3.0",
      "org.bitbucket.inkytonik.kiama" %% "kiama-extras" % "2.3.0",
      "com.novocode" % "junit-interface" % "0.11" % "test",
      "org.scala-sbt" %% "io" % "1.3.1" % "test"
    ),

    // We use the lib folder as resource directory to include it in the JAR
    Compile / unmanagedResourceDirectories += (baseDirectory in ThisBuild).value / "lib",

    Test / watchTriggers += baseDirectory.value.toGlob / "lib" / "**" / "*.effekt"
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq(
      "org.bitbucket.inkytonik.kiama" %%% "kiama-scalajs" % "2.4.0-SNAPSHOT"
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    // include all resource files in the virtual file system
    sourceGenerators in Compile += Def.task {

      val baseDir = (baseDirectory in ThisBuild).value / "lib"
      val resources = baseDir ** "*.*"

      val sourceDir = (sourceManaged in Compile).value
      val sourceFile = sourceDir / "Resources.scala"

      if (!sourceFile.exists() || sourceFile.lastModified() < baseDir.lastModified()) {

        val virtuals = resources.get.map { file =>
          val filename = file.relativeTo(baseDir).get
          val content = IO.read(file).replaceAllLiterally("$", "$$")
          s"""file(raw\"\"\"$filename\"\"\").write(raw\"\"\"$content\"\"\")"""
        }

        val scalaCode =
          s"""
package effekt.util
import effekt.util.JSPathUtils._

object Resources {

  def load() = {
${virtuals.mkString("\n\n")}
  }
}
"""

        IO.write(sourceFile, scalaCode)
      }

      Seq(sourceFile)
    }.taskValue,

    scalaJSUseMainModuleInitializer := true
  )
