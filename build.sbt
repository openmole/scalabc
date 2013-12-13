name := "scalabc"

organization := "fr.irstea"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalanlp" % "breeze-math_2.10" % "0.4"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.2" % "test"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

publishMavenStyle := true
