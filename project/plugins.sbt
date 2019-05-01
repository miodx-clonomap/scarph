//val ivyLocal = Resolver.file("local", file(Path.userHome.absolutePath + "/.ivy2/local"))(Resolver.ivyStylePatterns)
//externalResolvers := Seq(ivyLocal)

//resolvers ++= Seq(
//  "repo.jenkins-ci.org" at "https://repo.jenkins-ci.org/public"
//)

addSbtPlugin("clonomap" % "nice-sbt-settings" % "0.10.1")
