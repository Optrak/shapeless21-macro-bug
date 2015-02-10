organization := "com.optrak"

name := "shapeless-21-macro-bug"

version := "0.1-SNAPSHOT"

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots"),
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases")

scalaVersion := "2.11.5"

libraryDependencies ++= {
  Seq(
    "org.json4s" %% "json4s-native" % "3.2.11",
    "com.chuusai" %% "shapeless" % "2.1.0-RC2" changing()
  )
}

parallelExecution in Test := true

exportJars := true

scalacOptions ++= Seq("-feature", "-deprecation")
