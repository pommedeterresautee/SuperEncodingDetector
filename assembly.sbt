import sbtassembly.Plugin.AssemblyKeys
import AssemblyKeys._

assemblySettings

jarName in assembly := "SuperFileEncoding2.jar"

mainClass in assembly := Some("com.taj.unicode_detector.main")
