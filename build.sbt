//

val MyOrg = "io.github.kalin-rudnicki"

val MyScalaVersion = "2.13.4"
val CirceVersion = "0.14.0-M4"

val SharedSettings =
  Seq(
    organization := MyOrg,
    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("public"),
    ),
  )

lazy val `klib-webServer` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("klib-webServer"))
    .settings(
      name := "klib-webserver",
      version := "1.3.6",
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "shared" / "main" / "scala",
      libraryDependencies ++= Seq(
        MyOrg %%% "klib-core" % "1.2.5", // klib
        "io.github.cquiroz" %%% "scala-java-time" % "2.3.0",
        "com.lihaoyi" %%% "scalatags" % "0.9.2",
        "io.circe" %%% "circe-core" % CirceVersion,
        "io.circe" %%% "circe-generic" % CirceVersion,
        "io.circe" %%% "circe-parser" % CirceVersion,
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

lazy val `klib-webserver-plugin` =
  project
    .in(file("klib-webserver-plugin"))
    .enablePlugins(SbtPlugin)
    .settings(
      name := "klib-webserver-plugin",
      scalaVersion := "2.12.10",
      version := "0.0.2",
      addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.5.0"),
      SharedSettings,
    )

lazy val `klib-webserver-root` =
  project
    .in(file("."))
    .settings(
      publish / skip := true,
    )
    .aggregate(
      `klib-webServer`.js,
      `klib-webServer`.jvm,
      `klib-webserver-plugin`,
    )
