name := "scala-excel"

version := "1.0"

scalaVersion := "2.11.4"

scalacOptions ++= Seq("-feature")

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies += "org.scalafx" % "scalafx_2.11" % "8.0.20-R6"

libraryDependencies += "io.reactivex" %% "rxscala" % "0.23.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-pickling" % "0.10.0-SNAPSHOT"

resolvers += Resolver.sonatypeRepo("snapshots")

scalacOptions ++= Seq("-feature", "-deprecation", "-Xlint")

fork in run := true
