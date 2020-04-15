name := "scala-with-cats-code"
version := "0.1.0"

scalaVersion := "2.13.1"

scalacOptions ++= Seq(
  "-encoding",
  "UTF-8",        // source files are in UTF-8
  "-deprecation", // warn about use of deprecated APIs
  "-unchecked",   // warn about unchecked type parameters
  "-feature"      // warn about misused language features
  //"-Xlint",               // enable handy linter warnings
  // "-Ypartial-unification" // only 2.12 // allow the compiler to unify type constructors of different arities
  //"-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  // "-Xfatal-warnings",     // turn compiler warnings into errors
)

lazy val catsVersion             = "2.1.1"
lazy val silencerVersion         = "1.6.0"
lazy val kindProjectorVersion    = "0.11.0"
lazy val betterMonadicForVersion = "0.3.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "kittens"   % "2.0.0",
  "org.typelevel" %% "cats-free" % catsVersion,
  "org.typelevel" %% "cats-laws" % catsVersion % Test,
  // "org.typelevel" %% "discipline" % "0.10.0" % Test,
  "org.scalacheck"       %% "scalacheck"    % "1.14.3" % Test,
  "org.typelevel"        %% "cats-mtl-core" % "0.7.1",
  "com.github.mpilquist" %% "simulacrum"    % "0.19.0",
  "com.github.ghik"      % "silencer-lib"   % silencerVersion % Provided cross CrossVersion.full,
  // https://github.com/ghik/silencer
  "com.github.ghik" % "silencer-lib" % silencerVersion % Provided cross CrossVersion.full,
  compilerPlugin(
    "com.github.ghik" % "silencer-plugin" % silencerVersion cross CrossVersion.full
  ),
  // https://github.com/typelevel/kind-projector
  compilerPlugin(
    compilerPlugin("org.typelevel" % "kind-projector" % kindProjectorVersion cross CrossVersion.full)
  ),
  // https://github.com/oleg-py/better-monadic-for
  compilerPlugin(
    compilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForVersion)
  )
)
// addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
