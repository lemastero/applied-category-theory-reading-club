name := "applied-category-theory-reading-club"

version := "0.0.1"

scalaVersion := "2.13.3"

resolvers += Resolver.sonatypeRepo("snapshots")

lazy val catsVersion = "2.2.0"
lazy val catsMtlVersion = "1.0.0"
lazy val scalaTestPlusVersion = "3.1.0.0-RC2"
lazy val scalacheckVersion = "1.14.3"
lazy val silencerVersion = "1.7.1"
libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion withSources()
)

scalacOptions ++= Seq(
  "-encoding", "UTF-8"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)