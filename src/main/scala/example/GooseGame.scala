package example

import example.GameEngine.{Player, Position}

import scala.collection.immutable.ListMap
import scala.util.Random
import scala.util.matching.Regex


object GooseGame extends App {
  println("SIGTERM for exit")

  def printState: GameState => GameState = state => {
    Console.println(state.message); state
  }

  val stream = Stream.continually({ () => scala.io.StdIn.readLine })
  stream.foldLeft(GameState())(printState apply GameEngine.processInput(_, _))
}

object GameEngine {

  type Player = String
  type Message = String

  final case class Position(value: Int) extends AnyVal {
    def asString: String = value match {
      case 0 => "Start"
      case 6 => "The Bridge"
      case n@(5 | 9 | 14 | 18 | 23 | 27) => s"$n, The Goose"
      case v => v.toString
    }
  }

  val addPlayerRegex: Regex = """add player (\w+)$""".r
  val movePlayerRegex: Regex = """move (\w+) (\d+), (\d+)$""".r
  val movePlayerWithRollRegex: Regex = """move (\w+)$""".r

  val victory: Position = Position(63)
  val bridgeStart: Position = Position(6)
  val bridgeEnd: Position = Position(12)
  val goose: List[Position] = List(5, 9, 14, 18, 23, 27).map(Position)

  val random: Random = Random.self

  def processInput(currentState: GameState, input: () => String): GameState =
    input() match {
      case addPlayerRegex(name) => addPlayer(name, currentState)
      case movePlayerRegex(name, r1, r2) => movePlayer(name, r1.toInt, r2.toInt, currentState)
      case movePlayerWithRollRegex(name) => movePlayer(name, rollDice, rollDice, currentState)
      case _ => currentState.copy(message = "Unrecognized input")
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

    def handleBouncing: GameState = {
      val bouncedPosition = Position(victory.value - (newPosition.value - victory.value))
      val newMessage = s"$name rolls $rollOne, $rollTwo. $name moves from ${oldPosition.asString} " +
        s"to ${victory.asString}. $name bounces! $name returns to ${bouncedPosition.asString}"
      val players = state.players + (name -> bouncedPosition)
      state.copy(message = newMessage, players = players)
    }

    def findOthersAtPosition(s: GameState): Option[Player] = {
      val currentPlayerPosition = s.players(name)
      (s.players - name).find { case (_, p) => p == currentPlayerPosition }.map(_._1)
    }

    def applyModifications(s: GameState): GameState =
      s.players(name).value match {
        case victory.value => s.copy(message = s.message + s". $name Wins!!")
        case bridgeStart.value =>
          val modifiedState = s.copy(message = s.message + s". $name jumps to ${bridgeEnd.asString}",
            players = s.players + (name -> bridgeEnd))
          applyModifications(modifiedState)
        case v if goose.contains(Position(v)) =>
          val goosePosition = Position(s.players(name).value + rollOne + rollTwo)
          val modifiedState = s.copy(message = s.message + s". $name moves again and goes to ${goosePosition.asString}",
            players = s.players + (name -> goosePosition))
          applyModifications(modifiedState)
        case v if findOthersAtPosition(s).isDefined =>
          val maybeModifiedState = for {
            otherPlayerName <- findOthersAtPosition(s)
            otherPlayerPosition = s.players(otherPlayerName)
            otherPlayerUpdated = otherPlayerName -> otherPlayerPosition
            message = s". On ${Position(v).asString} there is $otherPlayerName, who returns to ${oldPosition.asString}"
          } yield s.copy(message = s.message + message, players = s.players + otherPlayerUpdated)
          maybeModifiedState.getOrElse(s)
        case _ => s
      }

    val intermediateState = if (newPosition.value > victory.value)
      handleBouncing
    else {
      val message = s"$name rolls $rollOne, $rollTwo. $name moves from ${oldPosition.asString} to ${newPosition.asString}"
      val players = state.players + (name -> newPosition)
      state.copy(message = message, players = players)
    }

    applyModifications(intermediateState)
  }

}

case class GameState(players: ListMap[Player, Position] = ListMap.empty, message: String = "")
