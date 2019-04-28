package example

import example.GameEngine.{Player, Position}

import scala.collection.immutable.ListMap
import scala.util.Random
import scala.util.matching.Regex


object Hello extends App {
  println("SIGTERM for exit")

  val stream = Stream.continually({ () => scala.io.StdIn.readLine })
  stream.foldLeft(GameState())(GameEngine.processInput)
}

object GameEngine {

  type Player = String
  type Message = String

  final case class Position(value: Int) extends AnyVal {
    def asString: String = value match {
      case 0 => "Start"
      case 6 => "The Bridge"
      case v => v.toString
    }
  }

  val addPlayerRegex: Regex = """add player (\w+)$""".r
  val movePlayerRegex: Regex = """move (\w+) (\d+), (\d+)$""".r
  val movePlayerWithRollRegex: Regex = """move (\w+)$""".r

  val victory: Position = Position(63)
  val bridgeStart: Position = Position(6)
  val bridgeEnd: Position = Position(12)

  val random: Random = Random.self

  def processInput(currentState: GameState, input: () => String): GameState = {
    val state = input() match {
      case addPlayerRegex(name) => addPlayer(name, currentState)
      case movePlayerRegex(name, r1, r2) => movePlayer(name, r1.toInt, r2.toInt, currentState)
      case movePlayerWithRollRegex(name) => movePlayer(name, rollDice, rollDice, currentState)
      case _ => currentState.copy(message = "Unrecognized input")
    }
    Console.println(state.message)
    state
  }

  def rollDice: Int = random.nextInt(6) + 1

  def addPlayer(name: Player, state: GameState): GameState =
    if (state.players.contains(name)) {
      state.copy(message = s"$name: already existing player")
    } else {
      val players = state.players + (name -> Position(0))
      state.copy(players = players, message = s"players: ${players.keySet.mkString(", ")}")
    }

  def movePlayer(name: Player,
                 rollOne: Int,
                 rollTwo: Int,
                 state: GameState): GameState = {
    val oldPosition = state.players(name)
    val newPosition = Position(oldPosition.value + rollOne + rollTwo)

    val intermediateState = if (newPosition.value > victory.value) {
      val bouncedPosition = Position(victory.value - (newPosition.value - victory.value))
      val newMessage = s"$name rolls $rollOne, $rollTwo. $name moves from ${oldPosition.asString} " +
        s"to ${victory.asString}. $name bounces! $name returns to ${bouncedPosition.asString}"
      val newPlayers = state.players + (name -> bouncedPosition)
      val newState = state.copy(message = newMessage, players = newPlayers)
      newState
    } else {
      val message = s"$name rolls $rollOne, $rollTwo. $name moves from ${oldPosition.asString} to ${newPosition.asString}"

      val newPlayers = state.players + (name -> newPosition)
      val newState = state.copy(message = message, players = newPlayers)
      newState
    }

    def applyModifications(s: GameState): GameState =
      s.players(name).value match {
        case victory.value => s.copy(message = s.message + s". $name Wins!!")
        case bridgeStart.value => s.copy(message = s.message + s". $name jumps to ${bridgeEnd.asString}",
          players = s.players + (name -> bridgeEnd))
        case _ => s
      }

    applyModifications(intermediateState)
  }

}

case class GameState(players: ListMap[Player, Position] = ListMap.empty, message: String = "")
