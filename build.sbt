organization := "edu.berkeley.cs"
version := "1.0-SNAPSHOT"
name := "cacl"
scalaVersion := "2.11.11"
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-language:reflectiveCalls")
libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.0.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
