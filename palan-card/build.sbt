name := "palan-card"

version := "0.1"

scalaVersion := "2.12.8"

scalacOptions ++= Seq(
  "-feature",
  "-language:postfixOps",
)

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.27"
libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.17"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5"