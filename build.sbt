//

val MyOrg = "io.github.kalin-rudnicki"

val MyScalaVersion = "2.13.4"
val CirceVersion = "0.14.0-M4"
val MonocleVersion = "3.0.0-M6"

val SharedSettings =
  Seq(
    organization := MyOrg,
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("public"),
    ),
  )

lazy val pye =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("pye"))
    .settings(
      name := "pye",
      version := "3.0.0",
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "shared" / "main" / "scala",
      libraryDependencies ++= Seq(
        MyOrg %%% "klib-core" % "1.3.3", // klib
        "io.github.cquiroz" %%% "scala-java-time" % "2.3.0",
        "com.lihaoyi" %%% "scalatags" % "0.9.2",
        "io.circe" %%% "circe-core" % CirceVersion,
        "io.circe" %%% "circe-generic" % CirceVersion,
        "io.circe" %%% "circe-parser" % CirceVersion,
        "com.github.julien-truffaut" %%% "monocle-core" % MonocleVersion,
        "com.github.julien-truffaut" %%% "monocle-macro" % MonocleVersion,
      ),
      scalaVersion := MyScalaVersion,
      SharedSettings,
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
      scalaVersion := "2.12.10",
      version := "0.0.2",
      addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.5.0"),
      SharedSettings,
    )

lazy val `pye-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
    )
    .aggregate(
      pye.js,
      pye.jvm,
      // `pye-plugin`,
    )
