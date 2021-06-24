//

val MyScalaVersion = "2.13.4"
val CirceVersion = "0.14.0-M4"

lazy val `klib-webServer` =
  crossProject(JSPlatform, JVMPlatform)
    .in(file("klib-webServer"))
    .settings(
      name := "klib-webserver",
      organization := "kalin-rudnicki",
      version := "0.7.5",
      unmanagedSourceDirectories in Compile +=
        baseDirectory.value / "shared" / "main" / "scala",
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %%% "scala-java-time" % "2.3.0",
        "kalin-rudnicki" %%% "klib-core" % "0.3.1", // klib
        "com.lihaoyi" %%% "scalatags" % "0.9.2",
        "io.circe" %%% "circe-core" % CirceVersion,
        "io.circe" %%% "circe-generic" % CirceVersion,
        "io.circe" %%% "circe-parser" % CirceVersion,
      ),
      scalaVersion := MyScalaVersion,
      resolvers += Resolver.mavenLocal,
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
        "org.webjars" % "bootstrap" % "3.2.0",
        "org.squeryl" %% "squeryl" % "0.9.15",
        "org.xerial" % "sqlite-jdbc" % "3.34.0",
      ),
    )

lazy val `klib-webServer-js` = `klib-webServer`.js
lazy val `klib-webServer-jvm` = `klib-webServer`.jvm
