resolvers += "Era7 maven releases" at "http://releases.era7.com.s3.amazonaws.com"

resolvers += "Era7 maven snapshots" at "http://snapshots.era7.com.s3.amazonaws.com"

addSbtPlugin("ohnosequences" % "nice-sbt-settings" % "0.5.0-SNAPSHOT")

// These versions fix the bug with unicode symbols:
addSbtPlugin("laughedelic" % "literator-plugin" % "0.5.2")

addSbtPlugin("com.markatta" % "taglist-plugin" % "1.3.1")
