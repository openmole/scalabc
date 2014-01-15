name := "scalabc"

organization := "fr.irstea"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalanlp" % "breeze-math_2.10" % "0.4"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>https://github.com/romainreuillon/scalabc/</url>
  <licenses>
    <license>
      <name>GNU General Public License (GPL)</name>
      <url>http://www.gnu.org/licenses/gpl.txt</url>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:romainreuillon/scalabc.git</url>
    <connection>scm:git:git@github.com:romainreuillon/scalabc.git</connection>
  </scm>
)

