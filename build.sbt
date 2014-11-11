name := "scala-excel"

version := "1.0"

scalaVersion := "2.11.2"

libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

libraryDependencies += "org.scalafx" % "scalafx_2.11" % "8.0.20-R6"

libraryDependencies += "com.netflix.rxjava" % "rxjava-scala" % "0.20.6"

fork in run := true