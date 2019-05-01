name          := "scarph"
organization  := "clonomap"
description   := "Scala graph API"
version       := "0.5.0"

libraryDependencies ++= Seq(
  "clonomap" %% "cosas"     % "0.10.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % Test
)

// shows time for each test:
testOptions in Test += Tests.Argument("-oD")

// publish settings
publishArtifact in (Test, packageBin) := true
bucketSuffix := "era7.com"

wartremoverErrors in (Compile, compile) := Seq()
wartremoverErrors in (Test, compile) := Seq()
// wartremoverErrors := Warts.allBut(Wart.Any)
