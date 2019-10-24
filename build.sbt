name := "scame"

version := "0.1"

libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "2.1.3",
  "dev.zio" %% "zio" % "1.0.0-RC14",

  // Scala Test
  "org.scalactic" %% "scalactic" % "3.0.8",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test")