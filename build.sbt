val http4s     = "1.0.0-M39"
val circe      = "0.14.5"
val doobie     = "1.0.0-RC2"
val pureconfig = "0.17.2"
val catsEffect = "3.4.8"
val fs2        = "3.6.1"
val jda        = "5.0.0-alpha.11"
val postgres   = "42.5.4"
val mysql      = "8.0.32"
val flyway     = "9.16.0"
val logback    = "1.4.6"
val log4cats   = "2.5.0"

val scala3Version = "3.2.2"
ThisBuild / scalaVersion := scala3Version

ThisBuild / publish / skip                      := true
ThisBuild / githubWorkflowJavaVersions          := Seq(JavaSpec.temurin("17"))
ThisBuild / crossScalaVersions                  := List(scala3Version)
ThisBuild / githubWorkflowIncludeClean          := false
ThisBuild / githubWorkflowTargetBranches        := Seq("master")
ThisBuild / githubWorkflowTargetPaths           := Paths.Include(List("**version.sbt"))
ThisBuild / githubWorkflowPublishTargetBranches := Seq(RefPredicate.Equals(Ref.Branch("master")))

ThisBuild / githubWorkflowPublishPreamble := Seq(
  WorkflowStep.Use(
    name = Some("Login to DockerHub"),
    ref = UseRef.Public("docker", "login-action", "v2"),
    params = Map(
      "username" -> "${{ secrets.DOCKERHUB_USERNAME }}",
      "password" -> "${{ secrets.DOCKERHUB_PASS }}",
    ),
  ),
)

ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("Docker / publish"),
    name = Some("Publish to docker hub"),
  ),
)

ThisBuild / githubWorkflowJobSetup ++= Seq(
  WorkflowStep.Sbt(
    List("+scalafmtCheckAll", "scalafmtSbtCheck"),
    name = Some("Scalafmt"),
  ),
)

lazy val dockerSettings = Seq(
  Docker / dockerRepository := Some("aifosi"),
  dockerUpdateLatest        := true,
  dockerBaseImage           := "openjdk:17-jdk",
  publish / skip            := false,
)

lazy val sharedSettings = Seq(
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
  .settings(
    name := "Chaster Discord Bots",
  )
  .aggregate(
    chaster,
    bot,
    lurch,
    bookOfAifosi,
  )

lazy val chaster = project
  .in(file("chaster"))
  .settings(
    name                 := "Chaster client",
    sharedSettings,
    libraryDependencies ++= Seq(
      "com.github.pureconfig" %% "pureconfig-core"        % pureconfig,
      "com.github.pureconfig" %% "pureconfig-cats-effect" % pureconfig,
      "org.typelevel"         %% "cats-effect"            % catsEffect,
      "co.fs2"                %% "fs2-core"               % fs2,
      // HTTP
      "org.http4s"            %% "http4s-dsl"             % http4s,
      "org.http4s"            %% "http4s-core"            % http4s,
      "org.http4s"            %% "http4s-circe"           % http4s,
      "org.http4s"            %% "http4s-ember-client"    % http4s,
      "io.circe"              %% "circe-core"             % circe,
      "io.circe"              %% "circe-parser"           % circe,
      // Logging
      "ch.qos.logback"         % "logback-classic"        % logback,
      "org.typelevel"         %% "log4cats-slf4j"         % log4cats,
    ),
  )

lazy val bot = project
  .in(file("bot"))
  .dependsOn(chaster)
  .settings(
    name                 := "Chaster Discord Bot",
    sharedSettings,
    libraryDependencies ++= Seq(
      // Discord
      "net.dv8tion"    % "JDA"                  % jda,
      // HTTP
      "org.http4s"    %% "http4s-ember-server"  % http4s,
      // DB
      "mysql"          % "mysql-connector-java" % mysql,
      "org.postgresql" % "postgresql"           % postgres,
      "org.flywaydb"   % "flyway-core"          % flyway,
      "org.tpolecat"  %% "doobie-core"          % doobie,
      "org.tpolecat"  %% "doobie-postgres"      % doobie,
    ),
  )

lazy val lurch = project
  .in(file("lurch"))
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .dependsOn(bot)
  .settings(
    name                     := "Lurch",
    Universal / javaOptions ++= Seq(
      "-Dconfig.file=/opt/docker/conf/application.conf",
    ),
    dockerSettings,
    sharedSettings,
  )

lazy val bookOfAifosi = project
  .in(file("bookofaifosi"))
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .dependsOn(bot)
  .settings(
    name                     := "BookOfAifosi",
    Universal / javaOptions ++= Seq(
      "-Dconfig.file=/opt/docker/conf/application.conf",
    ),
    dockerSettings,
    sharedSettings,
  )
