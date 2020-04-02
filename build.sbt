import sbtcrossproject.CrossProject
import scalariform.formatter.preferences.AlignSingleLineCaseStatements.MaxArrowIndent
import scalariform.formatter.preferences._

enablePlugins(ScalaJSPlugin)

lazy val effektVersion = "0.1.3"

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
    )
  )
  .jsSettings(
    // Add JS-specific settings here
    libraryDependencies ++= Seq(
      "org.bitbucket.inkytonik.kiama" %%% "kiama-scalajs" % "2.4.0-SNAPSHOT"
    ),
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },

    // include all resource files in the virtual file system
    sourceGenerators in Compile += Def.task {

      val baseDir = effekt.jvm.base / "src" / "main" / "resources" / "lib"
      val resources = baseDir ** "*.*"

      val sourceDir = (sourceManaged in Compile).value
      val sourceFile = sourceDir / "Resources.scala"

      if (!sourceFile.exists() || sourceFile.lastModified() < baseDir.lastModified()) {

        val virtuals = resources.get.map { file =>
          val filename = file.relativeTo(baseDir).get
          val content = IO.read(file).replaceAllLiterally("$", "$$")
          s"""VirtualFS.write(\"$filename\", raw\"\"\"$content\"\"\")"""
        }

        val scalaCode =
          s"""
package effekt.util
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


/**
 * Settings for sbt-microsite
 */
lazy val docs = project.in(file("docs"))
  .enablePlugins(MicrositesPlugin)
  .settings(commonSettings)
  .settings(micrositeSettings)
  .settings(noPublishSettings)

lazy val micrositeSettings = Seq(
  micrositeName := "Effekt Language",
  micrositeDescription := "A research language with effect handlers and lightweight effect polymorphism",
  micrositeAuthor := "the Effekt research team",
  micrositeHighlightLanguages ++= Seq("effekt"),
  micrositeDocumentationUrl := "docs",
  // micrositeBaseUrl := "/effekt",
  micrositeGithubOwner := "b-studios",
  micrositeGithubRepo := "effekt",
  micrositeHighlightTheme := "github-gist",
  micrositeOrganizationHomepage := "http://ps.informatik.uni-tuebingen.de/research/effects/",
  // micrositePushSiteWith := GitHub4s,
  // includeFilter in makeSite := "*.html" | "*.css" | "*.png" | "*.jpg" | "*.gif" | "*.js" | "*.swf" | "*.md",
  micrositePalette := effektTheme,
  fork in tut := true,
  git.remoteRepo := "git@github.com:b-studios/effekt.git"
)

lazy val effektTheme = Map(
    // https://colorhunt.co/palette/179481
    "brand-primary" -> "#542e5a",
    "brand-secondary" -> "#d73a49",
    "brand-tertiary" -> "#36bc98", //"#fb7b6b",
    "gray-dark" -> "#394A4B",
    "gray" -> "#e7d39f",
    "gray-light" -> "#CFE4E4",
    "gray-lighter" -> "#F4F4F4",
    "white-color" -> "#FFFFFF")
