organization := "org.clulab"
name := "timenorm"
version := "1.0.1"

scalaVersion := "2.12.8"
crossScalaVersions := List("2.11.12", "2.12.8", "2.13.0")
scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies ++= {
  val luceneVer = "6.6.6"

  Seq(
    "org.tensorflow"             % "tensorflow"      % "1.12.0",
    "org.clulab"                 % "timenorm-models" % "0.9.2",
    "commons-io"                 % "commons-io"      % "2.6",
    "org.scala-lang.modules"    %% "scala-xml"       % "1.2.0",
    "com.typesafe.play"         %% "play-json"       % "2.7.4",
    "org.scalatest"             %% "scalatest"       % "3.0.8" % Test,
    "com.lexicalscope.jewelcli"  % "jewelcli"        % "0.8.9" % Test,
    "org.timen"                  % "timen"           % "1.0.8" % Test,
  )
}

mainClass in (Compile, packageBin) := Some("org.clulab.timenorm.scate.TemporalNeuralParser")

// needed or tensorflow fails with "Cannot register 2 metrics with the same name"
Test / fork := true

// Additional metadata required by Sonatype OSS
// https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html
organizationName := "computational language understanding lab"
organizationHomepage := Some(url("http://clulab.org/"))

scmInfo := Some(
  ScmInfo(
    url("https://github.com/clulab/timenorm/"),
    "scm:git@github.com:clulab/timenorm.git"
  )
)
developers := List(
  Developer(
    id    = "bethard",
    name  = "Steven Bethard",
    email = "bethard@email.arizona.edu",
    url   = url("https://bethard.faculty.arizona.edu/")
  ),
  Developer(
    id    = "EgoLaparra",
    name  = "Egoitz Laparra",
    email = "laparra@email.arizona.edu",
    url   = url("http://clulab.cs.arizona.edu/people.php")
  ),
)

description := "Find and converts natural language expressions of dates and times to their normalized form."
licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
homepage := Some(url("https://github.com/example/project"))

// Remove all additional repository other than Maven Central from POM
pomIncludeRepository := { _ => false }
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
publishMavenStyle := true