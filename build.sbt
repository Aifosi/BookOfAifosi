val http4s = "1.0.0-M32"
val circe = "0.14.3"
val doobie = "1.0.0-RC2"
val pureconfig = "0.17.1"
val catsEffect = "3.3.14"
val fs2 = "3.3.0"
val jda = "5.0.0-alpha.11"
val postgres = "42.5.0"
val mysql = "8.0.30"
val flyway = "9.5.1"
val logback = "1.4.4"
val log4cats = "2.5.0"

lazy val dockerSettings = Seq(
  ThisBuild / Docker / dockerRepository := Some("aifosi"),
  ThisBuild / dockerUpdateLatest := true,
  ThisBuild / dockerBaseImage := "openjdk:17-jdk",
)

lazy val sharedSettings = Seq(
  scalaVersion := "3.2.0",
  // format: off
  javacOptions ++= Seq("-Xlint", "-encoding", "UTF-8"),
  scalacOptions ++= Seq(
    //"-explain",                          // Explain errors in more detail.
    //"-explain-types",                    // Explain type errors in more detail.
    "-indent",                           // Allow significant indentation.
    "-new-syntax",                       // Require `then` and `do` in control expressions.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.
    "-source:future",                    // better-monadic-for
    "-language:implicitConversions",     // Allow implicit conversions
    "-language:higherKinds",             // Allow higher-kinded types
    "-language:postfixOps",              // Explicitly enables the postfix ops feature
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-Xcheck-macros",
    //"-Xfatal-warnings",                  // Fail the compilation if there are any warnings.
    //"-Xmigration:3.1",                   // Warn about constructs whose behavior may have changed since version.
    //"-Xsemanticdb",                      // Store information in SemanticDB.
    //"-Xcheck-macros",
    //"-Ycook-comments",                   // Cook the comments (type check `@usecase`, etc.)
    //"-Yretain-trees",                    // Retain trees for top-level classes, accessible from ClassSymbol#tree
    //"-Yexplicit-nulls",                  // Make reference types non-nullable. Nullable types can be expressed with unions: e.g. String|Null.
    //"-Yshow-suppressed-errors",          // Also show follow-on errors and warnings that are normally suppressed.
    //"-rewrite",
    //"-source", "future-migration",
    //"-migration", "future-migration",
  )
  // format: on
)

lazy val root = project
  .in(file("."))
  .aggregate(
    bot,
    lurch,
  )

lazy val bot = project
  .in(file("bot"))
  .settings(
    name := "Chaster Discord Bot",
    resolvers += "jcenter-bintray" at "https://jcenter.bintray.com",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.github.pureconfig" %% "pureconfig-core"      % pureconfig,
      "org.typelevel"         %% "cats-effect"          % catsEffect,
      "co.fs2"                %% "fs2-core"             % fs2,
      //Discord
      "net.dv8tion"            % "JDA"                  % jda,
      //HTTP
      "org.http4s"            %% "http4s-dsl"           % http4s,
      "org.http4s"            %% "http4s-core"          % http4s,
      "org.http4s"            %% "http4s-circe"         % http4s,
      "org.http4s"            %% "http4s-blaze-server"  % http4s,
      "org.http4s"            %% "http4s-blaze-client"  % http4s,
      "io.circe"              %% "circe-core"           % circe,
      "io.circe"              %% "circe-parser"         % circe,
      //DB
      "org.postgresql"         % "postgresql"           % postgres,
      "mysql"                  % "mysql-connector-java" % mysql,
      "org.flywaydb"           % "flyway-core"          % flyway,
      "org.tpolecat"          %% "doobie-core"          % doobie,
      "org.tpolecat"          %% "doobie-postgres"      % doobie,
      //Logging
      "ch.qos.logback"         % "logback-classic"      % logback,
      "org.typelevel"         %% "log4cats-slf4j"       % log4cats,
    ),
  )

lazy val lurch = project
  .in(file("lurch"))
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .dependsOn(bot)
  .settings(
    name := "Lurch",
    Universal / javaOptions ++= Seq(
      "-Dconfig.file=/opt/docker/conf/application.conf",
    ),
    dockerSettings,
    sharedSettings,
  )

