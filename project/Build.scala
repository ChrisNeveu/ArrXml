import sbt._
import Keys._

object BuildSettings {

	val scalaV = "2.11.4"
	
	val buildSettings = Defaults.defaultSettings ++ Seq(
		organization	:= "arrxml",
		version			 := "0.0.1-SNAPSHOT",
		scalaVersion	:= scalaV
	)
}

object ArrXmlBuild extends Build {
	import BuildSettings._

	lazy val root: Project = Project(
		"root",
		file("."),
		settings = buildSettings ++ Seq(
			libraryDependencies ++= Seq(
				"org.jsoup" % "jsoup" % "1.8.1"))
	)
}
