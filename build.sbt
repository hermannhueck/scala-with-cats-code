name := "scala-with-cats-code"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.6"

scalacOptions ++= Seq(
  "-encoding", "UTF-8",   // source files are in UTF-8
  "-deprecation",         // warn about use of deprecated APIs
  "-unchecked",           // warn about unchecked type parameters
  "-feature",             // warn about misused language features
  //"-Xlint",               // enable handy linter warnings
  "-Ypartial-unification" // allow the compiler to unify type constructors of different arities
  //"-language:higherKinds",// allow higher kinded types without `import scala.language.higherKinds`
  // "-Xfatal-warnings",     // turn compiler warnings into errors
)

val catsVersion = "1.1.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "org.typelevel" %% "cats-free" % catsVersion,
  "org.typelevel" %% "cats-laws" % catsVersion % Test,
  "org.typelevel" %% "discipline" % "0.10.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
  "org.typelevel" %% "cats-mtl-core" % "0.3.0",
  "com.github.mpilquist" %% "simulacrum" % "0.12.0"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
