lazy val osNativeLibDir = (sys.props("os.name"), sys.props("os.arch")) match {
      case (os, arch) if os.contains("Mac") && arch.endsWith("64") => "macos64"
      case (os, arch) if os.contains("Linux") && arch.endsWith("64") => "linux64"
      case (os, arch) if os.contains("Windows") && arch.endsWith("32") => "windows32"
      case (os, arch) if os.contains("Windows") && arch.endsWith("64") => "windows64"
      case (os, arch) => sys.error("Unsupported OS [${os}] Architecture [${arch}] combo, OscaR currently supports macos64, linux64, windows32, windows64")
    }
    
lazy val root = (project in file(".")).
  settings(
    name := "Vastuu Santa Solver",
    scalaVersion := "2.11.4",
    resolvers += "Oscar Snapshots" at "http://artifactory.info.ucl.ac.be/artifactory/libs-release/",
    libraryDependencies += "oscar" %% "oscar-linprog" % "3.0.0" withSources(),
    resolvers += "Xypron Release" at "http://rsync.xypron.de/repository/",
    resolvers += "AWS S3 Release Repository" at "http://maven.leadoperations.co/release",
    resolvers += "Cognitive Computation Group" at "http://cogcomp.cs.illinois.edu/m2repo/",
    javaOptions in run += "-Djava.library.path=lib:lib/" + osNativeLibDir
  )

