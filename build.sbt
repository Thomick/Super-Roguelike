val swing = "org.scala-lang.modules" %% "scala-swing" % "2.0.1"
lazy val root = (project in file(".")).

settings(
    name := "Rogue project",
    libraryDependencies += swing,
    fork in run := true
)
