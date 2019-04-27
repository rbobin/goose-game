package example

object Hello extends App {
  println("SIGTERM for exit")
  while (true) {
    val output = readInput(scala.io.StdIn.readLine())
    Console.println(output)
  }

  def readInput(input: String): String =
    input match {
      case _ => "Unrecognized input"
    }
}
