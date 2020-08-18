val dottyVersion = "0.26.0-RC1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "profunctor-optics",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    scalacOptions ++= Seq(
      "-rewrite", "-indent",
      "-Yexplicit-nulls"
    ),

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
