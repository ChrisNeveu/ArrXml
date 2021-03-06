import sbt._
import Keys._
import wartremover._

object BuildSettings {

   val scalaV = "2.11.4"
   
   val buildSettings = Defaults.defaultSettings ++ Seq(
      organization := "org.arrxml"
    , version      := "0.0.1-SNAPSHOT"
    , scalaVersion := scalaV
    , scalacOptions ++= Seq(
         "-unchecked"
       , "-deprecation"
       , "-feature"
       , "-language:higherKinds"
       , "-language:postfixOps"
       , "-Xlint:_"
       , "-Ywarn-infer-any"
       )
    , wartremoverErrors ++= Warts.allBut(Wart.ListOps)
    )
}

object ArrXmlBuild extends Build {
   import BuildSettings._

   lazy val root: Project = Project(
      "root"
    , file(".")
    , settings = buildSettings ++ Seq(
         libraryDependencies ++= Seq(
            "org.jsoup" % "jsoup" % "1.8.1"
          , "org.scalaz" %% "scalaz-core" % "7.0.6"
          )
       )
    )
}
