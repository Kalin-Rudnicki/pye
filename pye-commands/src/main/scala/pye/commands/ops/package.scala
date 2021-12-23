package pye.commands

package object ops {

  def stringToLines(string: String): List[String] = {
    val split = string.split("\n")
    if (string.endsWith("\n")) split.appended("").toList
    else split.toList
  }

}
