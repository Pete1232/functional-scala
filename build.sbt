name := "FunctionalProgrammingInScala"

version := "1.0"

scalaVersion := "2.11.8"

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.0",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.scalamock" %% "scalamock-scalatest-support" % "3.4.2" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
  "org.mockito" % "mockito-core" % "2.6.4" % "test"
)
