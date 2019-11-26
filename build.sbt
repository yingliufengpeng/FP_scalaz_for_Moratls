name := "Functional_Programming_for_Mortals"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions in ThisBuild ++= Seq(
  "-language:_",
  "-Ypartial-unification",
  "-Xfatal-warnings",
  "-language:higherKinds"
)

libraryDependencies ++= Seq(
  "com.github.mpilquist" %% "simulacrum"     % "0.13.0",
  "org.scalaz"           %% "scalaz-core"    % "7.2.26"
)

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
