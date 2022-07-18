name := "BookOfAifosi"

scalaVersion := "3.1.3"

// format: off
javacOptions ++= Seq("-Xlint", "-encoding", "UTF-8")
scalacOptions ++= Seq(
  "-explain",                          // Explain errors in more detail.
  "-explain-types",                    // Explain type errors in more detail.
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

resolvers += "jcenter-bintray" at "https://jcenter.bintray.com"

enablePlugins(JavaAppPackaging)
enablePlugins(DockerPlugin)

val http4sVersion = "1.0.0-M32"
val circeVersion = "0.14.2"
val doobieVersion = "1.0.0-RC2"
libraryDependencies ++= Seq(
  "net.dv8tion"            % "JDA"                 % "5.0.0-alpha.11",
  "com.github.pureconfig" %% "pureconfig-core"     % "0.17.1",
  "org.typelevel"         %% "cats-effect"         % "3.3.12",
  "co.fs2"                %% "fs2-core"            % "3.2.8",
  "org.http4s"            %% "http4s-dsl"          % http4sVersion,
  "org.http4s"            %% "http4s-core"         % http4sVersion,
  "org.http4s"            %% "http4s-circe"        % http4sVersion,
  "org.http4s"            %% "http4s-blaze-server" % http4sVersion,
  "org.http4s"            %% "http4s-blaze-client" % http4sVersion,
  "io.circe"              %% "circe-core"          % circeVersion,
  "io.circe"              %% "circe-parser"        % circeVersion,
  "org.postgresql"         % "postgresql"          % "42.3.6",
  "org.flywaydb"           % "flyway-core"         % "8.5.12",
  "org.tpolecat"          %% "doobie-core"         % doobieVersion,
  "org.tpolecat"          %% "doobie-postgres"     % doobieVersion,
  "ch.qos.logback"         % "logback-classic"     % "1.2.11",
  "org.typelevel"         %% "log4cats-slf4j"      % "2.3.0",
)

Docker / dockerRepository := Some("aifosi")

dockerUpdateLatest := true
