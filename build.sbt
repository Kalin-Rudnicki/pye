//

val Scala_2_12 = "2.12.10"
val Scala_2_13 = "2.13.4"
val MonocleVersion = "3.0.0-M6"
val KlibVersion = "2.0.7"

val MyOrg = "io.github.kalin-rudnicki"
val githubUsername = "Kalin-Rudnicki"
val githubProject = "pye"

ThisBuild / dynverVTagPrefix := false
ThisBuild / dynverSonatypeSnapshots := true
ThisBuild / watchBeforeCommand := Watch.clearScreen

ThisBuild / version ~= (_.replace('+', '-'))
ThisBuild / dynver ~= (_.replace('+', '-'))

// =====|  |=====

inThisBuild(
  Seq(
    organization := MyOrg,
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("public"),
    ),
    //
    description := "Webserver/frontend for ScalaJS.",
    licenses := List("MIT" -> new URL("https://opensource.org/licenses/MIT")),
    homepage := Some(url(s"https://github.com/$githubUsername/$githubProject")),
    developers := List(
      Developer(
        id = "Kalin-Rudnicki",
        name = "Kalin Rudnicki",
        email = "kalin.rudnicki@gmail.com",
        url = url(s"https://github.com/$githubUsername"),
      ),
    ),
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  ),
)

// =====|  |=====

lazy val pye =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("pye"))
    .settings(
      name := "pye",
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "shared" / "main" / "scala",
      libraryDependencies ++= Seq(
        MyOrg %%% "klib-core" % KlibVersion,
        "io.github.cquiroz" %%% "scala-java-time" % "2.3.0",
        "com.lihaoyi" %%% "scalatags" % "0.9.2",
        "com.github.julien-truffaut" %%% "monocle-core" % MonocleVersion,
        "com.github.julien-truffaut" %%% "monocle-macro" % MonocleVersion,
      ),
      scalaVersion := Scala_2_13,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      // NOTE : Manually set version
      // version := "4.5.3",
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom" % "1.1.0",
      ),
    )
    .jvmSettings(
      libraryDependencies ++= Seq(
        "org.eclipse.jetty" % "jetty-servlet" % "11.0.0",
        "org.eclipse.jetty" % "jetty-server" % "11.0.0",
        "org.squeryl" %% "squeryl" % "0.9.15",
        "org.xerial" % "sqlite-jdbc" % "3.34.0",
      ),
    )

lazy val `pye-plugin` =
  project
    .in(file("pye-plugin"))
    .enablePlugins(SbtPlugin)
    .settings(
      name := "pye-plugin",
      scalaVersion := Scala_2_12,
      version := "0.1.0",
      addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.5.0"),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      publish / skip := true,
    )

lazy val `pye-commands` =
  project
    .in(file("pye-commands"))
    .settings(
      name := "pye-commands",
      scalaVersion := Scala_2_13,
      version := "0.0.1",
      libraryDependencies ++= Seq(
        MyOrg %% "klib-core" % KlibVersion,
      ) ++ Seq(
        "org.scalatest" %% "scalatest" % "3.2.3",
      ).map(_ % Test),
      sonatypeCredentialHost := "s01.oss.sonatype.org",
      publish / skip := true,
      assembly / assemblyJarName := "pye-commands.jar",
      assembly / mainClass := Some("pye.commands.Main"),
      assembly / assemblyOutputPath := file("pye-commands.jar"),
    )

lazy val `pye-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
      sonatypeCredentialHost := "s01.oss.sonatype.org",
    )
    .aggregate(
      pye.js,
      pye.jvm,
      `pye-plugin`,
      `pye-commands`,
    )
