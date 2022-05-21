import sbtcrossproject.CrossProject
import scalariform.formatter.preferences.AlignSingleLineCaseStatements.MaxArrowIndent
import scalariform.formatter.preferences._

import scala.sys.process.Process

// additional targets that can be used in sbt
lazy val deploy = taskKey[Unit]("Builds the jar and moves it to the bin folder")
lazy val generateLicenses = taskKey[Unit]("Analyses dependencies and downloads all licenses")
lazy val updateVersions = taskKey[Unit]("Update version in package.json and pom.xml")
lazy val install = taskKey[Unit]("Installs the current version locally")
lazy val assembleBinary = taskKey[Unit]("Assembles the effekt binary in bin/effekt")


lazy val effektVersion = "0.1.16"

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
)

lazy val commonSettings = Seq(
  scalaVersion := "3.1.2",
  scalacOptions ++= Seq(
    "-encoding", "utf8",
    "-deprecation",
    "-unchecked",
    // "-Xlint",
    //TODO "-Xfatal-warnings",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions"
  ),
  scalariformPreferences := scalariformPreferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(DanglingCloseParenthesis, Force)
    .setPreference(NewlineAtEndOfFile, true)
    .setPreference(MaxArrowIndent, 20)
)

enablePlugins(ScalaJSPlugin)

lazy val replDependencies = Seq(
  "jline" % "jline" % "2.14.6",
  "org.rogach" %% "scallop" % "4.1.0",
)

lazy val lspDependencies = Seq(
  "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.12.0",
  "com.google.code.gson" % "gson" % "2.8.2"
)

lazy val testingDependencies = Seq(
  "org.scala-sbt" %% "io" % "1.6.0" % "test",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test"
)

lazy val kiama: CrossProject = crossProject(JSPlatform, JVMPlatform).in(file("kiama"))
  .settings(commonSettings)
  .settings(noPublishSettings)
  .settings(
    name := "kiama"
  )
  .jvmSettings(
    libraryDependencies ++= (replDependencies ++ lspDependencies)
  )

lazy val root = project.in(file("effekt"))
  .aggregate(effekt.js, effekt.jvm)
  .settings(noPublishSettings)
  .settings(Seq(
    Compile / run := (effekt.jvm / Compile / run).evaluated
  ))



lazy val effekt: CrossProject = crossProject(JSPlatform, JVMPlatform).in(file("effekt"))
  .settings(
    name := "effekt",
    version := effektVersion
  )
  .settings(commonSettings)
  .dependsOn(kiama)
  // .enablePlugins(NativeImagePlugin)
  .jvmSettings(
    libraryDependencies ++= (replDependencies ++ lspDependencies ++ testingDependencies),

    // Test configuration
    // ------------------
    Test / parallelExecution := false,

    Test / watchTriggers += baseDirectory.value.toGlob / "libraries" / "**" / "*.effekt",

    // show duration of the tests
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-oD"),

    // disable tests for assembly to speed up build
    assembly / test := {},


    // Options to compile Effekt with native-image
    // -------------------------------------------
    //    nativeImageOptions ++= Seq(
    //      "--no-fallback",
    //      "--initialize-at-build-time",
    //      "--report-unsupported-elements-at-runtime",
    //      "-H:+ReportExceptionStackTraces",
    //      "-H:IncludeResourceBundles=jline.console.completer.CandidateListCompletionHandler",
    //      "-H:ReflectionConfigurationFiles=../../native-image/reflect-config.json",
    //      "-H:DynamicProxyConfigurationFiles=../../native-image/dynamic-proxies.json"
    //    ),


    // Assembling one big jar-file and packaging it
    // --------------------------------------------
    assembly / mainClass := Some("effekt.Server"),

    assembly / assemblyJarName := "effekt.jar",

    // we use the lib folder as resource directory to include it in the JAR
    Compile / unmanagedResourceDirectories += (ThisBuild / baseDirectory).value / "libraries",

    Compile / unmanagedResourceDirectories += (ThisBuild / baseDirectory).value / "licenses",


    assembleBinary := {
      val jarfile = assembly.value

      // prepend shebang to make jar file executable
      val binary = (ThisBuild / baseDirectory).value / "bin" / "effekt"
      IO.delete(binary)
      IO.append(binary, "#! /usr/bin/env java -jar\n")
      IO.append(binary, IO.readBytes(jarfile))
    },

    deploy := {
      generateLicenses.value
      updateVersions.value
      assembleBinary.value
    },

    install := {
      assembleBinary.value
      Process("npm pack").!!
      Process(s"npm install -g effekt-${effektVersion}.tgz").!!
    },

    generateLicenses := {
      Process("mvn license:download-licenses license:add-third-party").!!

      val kiamaFolder = (ThisBuild / baseDirectory).value / "kiama"
      val licenseFolder = (ThisBuild / baseDirectory).value / "licenses"
      IO.copyFile(kiamaFolder / "LICENSE", licenseFolder / "kiama-license.txt")
      IO.copyFile(kiamaFolder / "README.md", licenseFolder / "kiama-readme.txt")
    },

    updateVersions := {
      Process(s"npm version ${effektVersion} --no-git-tag-version --allow-same-version").!!
      Process(s"mvn versions:set -DnewVersion=${effektVersion} -DgenerateBackupPoms=false").!!
    },

    Compile / sourceGenerators += versionGenerator.taskValue
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    // include all resource files in the virtual file system
    Compile / sourceGenerators += stdLibGenerator.taskValue
  )

lazy val versionGenerator = Def.task {
  val sourceDir = (Compile / sourceManaged).value
  val sourceFile = sourceDir / "effekt" / "util" / "Version.scala"

  IO.write(sourceFile,
    s"""package effekt.util
       |
       |object Version {
       |  val effektVersion = \"${effektVersion}\"
       |}
       |""".stripMargin)

  Seq(sourceFile)
}

/**
 * This generator is used by the JS version of our compiler to bundle the
 * Effekt standard into the JS files and make them available in the virtual fs.
 */
lazy val stdLibGenerator = Def.task {

  val baseDir = (ThisBuild / baseDirectory).value / "libraries" / "js" / "monadic"
  val resources = baseDir ** "*.*"

  val sourceDir = (Compile / sourceManaged).value
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
import effekt.util.paths._

object Resources {

  def load() = {
${virtuals.mkString("\n\n")}
  }
}
"""

    IO.write(sourceFile, scalaCode)
  }

  Seq(sourceFile)
}
