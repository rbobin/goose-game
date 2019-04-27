package example


object Hello extends App {
  println("SIGTERM for exit")

  type Player = String

  def addPlayer(name: String): String = ???

  val stream = Stream.continually({ () => scala.io.StdIn.readLine })

  stream.foldLeft(GameState())(GameEngine.processInput)
}

object GameEngine {

  def processInput(currentState: GameState, input: () => String): GameState = {
    val (state, output) = input() match {
      case _ => (GameState(), "Unrecognized input")
    }
    Console.println(output)
    state
  }

}

case class GameState()