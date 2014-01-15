name := "scalabc"

organization := "fr.irstea"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalanlp" % "breeze-math_2.10" % "0.4"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test"

credentials += Credentials(Path.userHome / ".sbt" / "iscpif.credentials")

publishTo <<= version { (v: String) =>
  val maven = "http://maven.iscpif.fr/"
  if (v.trim.endsWith("SNAPSHOT")) Some("snapshots" at maven + "snapshots")
  else Some("releases"  at maven + "releases")
}

