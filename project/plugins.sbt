logLevel := Level.Warn
// Produce test coverage reports
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.3.5")
// Scala code linter
addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")
// Flag errors in ScalaTest code at compile time
resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"
addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.0")