import sbtcrossproject.CrossProject

import scala.sys.process.Process

// additional targets that can be used in sbt
lazy val deploy = taskKey[Unit]("Builds the jar and moves it to the bin folder")
lazy val generateLicenses = taskKey[Unit]("Analyses dependencies and downloads all licenses")
lazy val updateVersions = taskKey[Unit]("Update version in package.json and pom.xml")
lazy val install = taskKey[Unit]("Installs the current version locally")
lazy val assembleBinary = taskKey[Unit]("Assembles the effekt binary in bin/effekt")
lazy val downloadJITBinary = taskKey[Unit]("Downloads the current JIT binaries to bin")

lazy val effektVersion = "0.2.0"

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
)

lazy val commonSettings = Seq(
  scalaVersion := "3.1.3",
  scalacOptions ++= Seq(
    "-encoding", "utf8",
    "-deprecation",
    "-unchecked",
    // "-Xlint",
    "-Xfatal-warnings",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions"
  )
)

enablePlugins(ScalaJSPlugin)

lazy val replDependencies = Seq(
  "jline" % "jline" % "2.14.6",
  "org.rogach" %% "scallop" % "4.1.0",
)

lazy val lspDependencies = Seq(
  "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "0.12.0",
  "com.google.code.gson" % "gson" % "2.8.9"
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

    downloadJITBinary := {
      import scala.util.matching.Regex
      val log = sbt.Keys.streams.value.log

      // determine OS and architecture
      var arch = ""
      var os = ""
      Process("uname -m").!(sys.process.ProcessLogger({x => arch=x}))
      Process("uname -s").!(sys.process.ProcessLogger({x => os=x}))

      // Try downloading binary
      log.info("Downloading binary for %s-%s".format(arch,os))
      val bindir =  (ThisBuild / baseDirectory).value / "bin" / ("%s-%s".format(arch,os))
      val binname = bindir / "rpyeffect-jit"

      // Give (hopefully) useful error messages
      val ghEC = try {
        Process("gh release download -R se-tuebingen/jitting-effects latest -p rpyeffect-jit-%s-%s".format(arch, os)).!
      } catch {
        case e: java.io.IOException => {
          if(e.getCause.isInstanceOf[java.io.IOException]
            && "error=2,.*".r.findPrefixMatchOf(e.getCause.getMessage).isDefined) {
            2
          } else throw e
        }
      }
      if (ghEC != 0) {
        ghEC match {
          case 2 => {
            log.error("Downloading the JIT binary requires the `gh` cli tool (see cli.github.com).")
          }
          case 4 => {
            log.error("As of now, the JIT binary is only available for members of the se-tuebingen org. ")
            log.error("Downloading it requires a GitHub personal access token in GH_TOKEN. " +
              "Alternatively, run `gh auth login` first.")
          }
          case _ => {
            log.error("Download failed (error=%d)".format(ghEC))
          }
        }
      } else {
        Process("mkdir -p " + bindir).!
        Process("mv rpyeffect-jit-%s-%s ".format(arch,os) + binname).!
        Process("chmod +x " + binname).!
      }
    },

    deploy := {
      generateLicenses.value
      updateVersions.value
      assembleBinary.value
    },

    install := {
      assembleBinary.value
      downloadJITBinary.value

      Process(s"${npm.value} pack").!!
      Process(s"${npm.value} install -g effekt-${effektVersion}.tgz").!!
    },

    generateLicenses := {
      Process(s"${mvn.value} license:download-licenses license:add-third-party").!!

      val kiamaFolder = (ThisBuild / baseDirectory).value / "kiama"
      val licenseFolder = (ThisBuild / baseDirectory).value / "licenses"
      IO.copyFile(kiamaFolder / "LICENSE", licenseFolder / "kiama-license.txt")
      IO.copyFile(kiamaFolder / "README.md", licenseFolder / "kiama-readme.txt")
    },

    updateVersions := {
      Process(s"${npm.value} version ${effektVersion} --no-git-tag-version --allow-same-version").!!
      Process(s"${mvn.value} versions:set -DnewVersion=${effektVersion} -DgenerateBackupPoms=false").!!
    },

    Compile / sourceGenerators += versionGenerator.taskValue
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    // include all resource files in the virtual file system
    Compile / sourceGenerators += stdLibGenerator.taskValue
  )


lazy val platform = Def.task {
  val platformString = System.getProperty("os.name").toLowerCase
  if (platformString.contains("win")) "windows"
  else if (platformString.contains("mac")) "macos"
  else if (platformString.contains("linux")) "linux"
  else sys error s"Unknown platform ${platformString}"
}

lazy val npm = Def.task {
  if (platform.value == "windows") "npm.cmd" else "npm"
}

lazy val mvn = Def.task {
  if (platform.value == "windows") "mvn.cmd" else "mvn"
}


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
