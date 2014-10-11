import scalariform.formatter.preferences._

scalaVersion := "2.11.2"

crossScalaVersions := Seq("2.10.4", "2.11.2")

scalariformSettings

ScalariformKeys.preferences := 
  ScalariformKeys.preferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(RewriteArrowSymbols, true)
  
organization := "fr.iscpif"

name := "scalabc"

resolvers += "ISC-PIF" at "http://maven.iscpif.fr/public/"

publishTo <<= isSnapshot { snapshot =>
  val nexus = "https://oss.sonatype.org/"
  if (snapshot) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

  
libraryDependencies += "org.scalanlp" %% "breeze" % "0.9"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.0" % "test"

pomIncludeRepository := { _ => false }

licenses := Seq("Affero GPLv3" -> url("http://www.gnu.org/licenses/"))

homepage := Some(url("https://github.com/openmole/scalabc"))

scmInfo := Some(ScmInfo(url("https://github.com/openmole/mgo.git"), "scm:git:git@github.com:openmole/mgo.git"))

pomExtra := (
  <developers>
    <developer>
      <id>romainreuillon</id>
      <name>Romain Reuillon</name>
    </developer>
    <developer>
      <id>guillaumecherel</id>
      <name>Guillaume Cherel</name>
    </developer>
  </developers>
)


