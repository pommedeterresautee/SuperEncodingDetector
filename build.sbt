name := "unicode_detector"

version := "1.0"

scalaVersion := "2.10.2"

//resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.2.3"
//  "org.scalaz.stream" %% "scalaz-stream" % "0.3.1"
)

