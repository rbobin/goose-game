package example

import example.GameEngine.Position
import org.scalacheck.Prop.BooleanOperators
import org.specs2.{Specification, _}

import scala.collection.immutable._

class HelloSpec extends Specification with ScalaCheck {
  def is =
    s2"""

  "add player" command:
    adds a player if there is no such name $addPlayer1
    does not add a player if there is such name $addPlayer2

  "move player" command:
    moves the player $movePlayer1
    follows specification $movePlayerSpec
    player that reaches victory position wins $movePlayer2
    follows victory specification $victorySpec
    follows bouncing specification $bounceSpec
    follows dice roll specification $diceRollSpec
    follows "The Bridge" specification $bridgeSpec
  """

  private def addPlayer1 =
    prop { (players: Set[String], player: String) =>
      !players.contains(player) ==> {
        val state = GameState(ListMap.empty ++ players.toList.map(n => n -> Position(0)))
        val changedState = GameEngine.addPlayer(player, state)

        (changedState.players.keySet mustEqual players + player) and
          (changedState.message mustEqual s"players: ${(players.toList :+ player).mkString(", ")}")
      }
    }

  private def addPlayer2 =
    prop { players: List[String] =>
      players.nonEmpty ==> {
        val state = GameState(ListMap.empty ++ players.map(n => n -> Position(0)))
        val changedState = GameEngine.addPlayer(players.head, state)

        (changedState.players.keySet mustEqual players.toSet) and
          (changedState.message mustEqual s"${players.head}: already existing player")
      }
    }

  private def movePlayer1 =
    prop { (player: String, rollOne: Int, rollTwo: Int) =>
      (player.nonEmpty && Int.MinValue + rollOne + rollTwo < 0) ==> {
        val state = GameState(ListMap.empty + (player -> Position(Int.MinValue)))
        val changedState = GameEngine.movePlayer(player, rollOne, rollTwo, state)

        (changedState.players(player).value mustEqual Int.MinValue + rollOne + rollTwo) and
          (changedState.message mustEqual s"$player rolls $rollOne, $rollTwo. $player moves " +
            s"from ${state.players(player).asString} to ${changedState.players(player).asString}")
      }
    }

  private def movePlayer2 =
    prop { (player: String, rollOne: Int, rollTwo: Int) =>
      player.nonEmpty ==> {
        val startingPosition = Position(GameEngine.victory.value - rollOne - rollTwo)
        val state = GameState(ListMap.empty + (player -> startingPosition))
        val changedState = GameEngine.movePlayer(player, rollOne, rollTwo, state)

        (changedState.players(player).value mustEqual GameEngine.victory.value) and
          (changedState.message mustEqual s"$player rolls $rollOne, $rollTwo. $player moves " +
            s"from ${startingPosition.asString} to ${changedState.players(player).asString}. $player Wins!!")
      }
    }

  private def movePlayerSpec = {
    val state = GameState(ListMap.empty ++ List("Pippo" -> Position(0), "Pluto" -> Position(0)))

    val state1 = GameEngine.processInput(state, { () => "move Pippo 4, 3" })
    state1.message mustEqual "Pippo rolls 4, 2. Pippo moves from Start to 7"

    val state2 = GameEngine.processInput(state1, { () => "move Pluto 2, 2" })
    state2.message mustEqual "Pluto rolls 2, 2. Pluto moves from Start to 4"

    val state3 = GameEngine.processInput(state2, { () => "move Pippo 2, 3" })
    state3.message mustEqual "Pippo rolls 2, 3. Pippo moves from 7 to 12"
  }

  private def victorySpec = {
    val state = GameState(ListMap.empty ++ List("Pippo" -> Position(60)))

    val newState = GameEngine.processInput(state, { () => "move Pippo 1, 2" })
    newState.message mustEqual "Pippo rolls 1, 2. Pippo moves from 60 to 63. Pippo Wins!!"
  }

  private def bounceSpec = {
    val state = GameState(ListMap.empty ++ List("Pippo" -> Position(60)))

    val newState = GameEngine.processInput(state, { () => "move Pippo 3, 2" })
    newState.message mustEqual "Pippo rolls 3, 2. Pippo moves from 60 to 63. Pippo bounces! Pippo returns to 61"
  }

  private def diceRollSpec = {
    val state = GameState(ListMap.empty ++ List("Pippo" -> Position(4)))

    val newState = GameEngine.processInput(state, { () => "move Pippo" })
    newState.message must contain("Pippo rolls")
    newState.message must contain("Pippo moves")
  }

  private def bridgeSpec = {
    val state = GameState(ListMap.empty ++ List("Pippo" -> Position(4)))

    val newState = GameEngine.processInput(state, { () => "move Pippo 1, 1" })
    newState.message mustEqual "Pippo rolls 1, 1. Pippo moves from 4 to The Bridge. Pippo jumps to 12"
  }
}
