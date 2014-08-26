
import sbt._
import Keys._
import com.typesafe.sbt.SbtScalariform._
import scalariform.formatter.preferences._

object ABCBuild extends Build {

   override def settings = super.settings ++ Seq (
    scalaVersion := "2.11.2",
    crossScalaVersions := Seq("2.10.4", "2.11.2")
  )

  lazy val defaultSettings =
    settings ++
      scalariformSettings ++ Seq(
    ScalariformKeys.preferences :=
      ScalariformKeys.preferences.value
        .setPreference(AlignSingleLineCaseStatements, true)
        .setPreference(RewriteArrowSymbols, true),
    organization := "fr.iscpif",
    resolvers += "ISC-PIF" at "http://maven.iscpif.fr/public/",
    publishTo <<= isSnapshot { snapshot =>
      val nexus = "https://oss.sonatype.org/"
      if (snapshot) Some("snapshots" at nexus + "content/repositories/snapshots")
      else Some("releases" at nexus + "service/local/staging/deploy/maven2")
    }
    )

  
  lazy val abc = Project(id = "abc", base = file("scalabc")) settings (
    libraryDependencies += "org.scalanlp" %% "breeze" % "0.9",
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3",
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"
    )

}
