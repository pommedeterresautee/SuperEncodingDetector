import sbtassembly.Plugin.AssemblyKeys
import AssemblyKeys._

assemblySettings

jarName in assembly := "SuperFileEncoding.jar"

mainClass in assembly := Some("com.taj.unicode_detector.Main")


