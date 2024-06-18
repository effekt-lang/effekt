package effekt
package util

enum OS {
  case POSIX, Windows
}

/**
 * Returns an enum determining the OS we are running on.
 */
lazy val os: OS = {
  val name = System.getProperty("os.name").toLowerCase()
  if (name.contains("win")) {
    OS.Windows
  } else {
    OS.POSIX // assume: There are no other OSs we currently support
  }
}

