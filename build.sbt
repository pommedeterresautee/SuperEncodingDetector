import sbtassembly.Plugin._
import sbtassembly.Plugin.AssemblyKeys
import AssemblyKeys._
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._
import sbtrelease.ReleasePlugin._
import sbtrelease.ReleasePlugin.ReleaseKeys._
import sbtrelease._
import ReleaseStateTransformations._

name := "super_encoding_detector"

version := "1.0.3"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.2",
  "com.ibm.icu" % "icu4j" % "53.1",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  "org.slf4j" % "slf4j-simple" % "1.7.7",
  "org.rogach" %% "scallop" % "0.9.5",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.2" % "test",
  "org.scalatest" %% "scalatest" % "2.1.4" % "test",
  "commons-codec" % "commons-codec" % "1.9" % "test"
)

scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
  "-language:reflectiveCalls",
  "-Ywarn-dead-code",
  "-Ywarn-inaccessible",
  "-Ywarn-nullary-unit",
  "-Ywarn-nullary-override",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(AlignParameters, true)
  .setPreference(CompactControlReadability, true)
  .setPreference(IndentLocalDefs, true)
  .setPreference(PreserveDanglingCloseParenthesis, true)
  .setPreference(RewriteArrowSymbols, true)
  .setPreference(AlignSingleLineCaseStatements.MaxArrowIndent, 100)

releaseSettings

useGlobalVersion := false

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  setNextVersion,
  commitNextVersion,
  pushChanges
)

incOptions := incOptions.value.withNameHashing(nameHashing = true)

assemblySettings

jarName in assembly := s"${name.value}_${version.value}.jar"

mainClass in assembly := Some("com.taj.unicode_detector.CommandLine.Main")