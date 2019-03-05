import Dependencies._

ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.github.arturopala"
ThisBuild / organizationName := "arturopala"
ThisBuild / startYear := Some(2019)

lazy val root = (project in file("."))
  .enablePlugins(AutomateHeaderPlugin)
  .settings(
    name := "hackos",
    licenses += ("Apache-2.0", new URL("https://www.apache.org/licenses/LICENSE-2.0.txt")),
    libraryDependencies += scalaTest % Test
  )