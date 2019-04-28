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
    prop { (player: String, startingPosition: Int, diceRollOne: Int, diceRollTwo: Int) =>
      (player.nonEmpty && startingPosition + diceRollOne + diceRollTwo < Int.MaxValue) ==> {
        val state = GameState(ListMap.empty + (player -> Position(startingPosition)))
        val changedState = GameEngine.movePlayer(player, diceRollOne, diceRollTwo, state)

        (changedState.players(player).value mustEqual startingPosition + diceRollOne + diceRollTwo) and
          (changedState.message mustEqual s"$player rolls $diceRollOne, $diceRollTwo. $player moves " +
            s"from ${state.players(player).asString} to ${changedState.players(player).asString}")
      }
    }

  private def movePlayerSpec = {
    val pippo = "Pippo"
    val pluto = "Pluto"
    val state = GameState(ListMap.empty ++ List(pippo -> Position(0), pluto -> Position(0)))

    val state1 = GameEngine.processInput(state, { () => "move Pippo 4, 2" })
    state1.message mustEqual "Pippo rolls 4, 2. Pippo moves from Start to 6"

    val state2 = GameEngine.processInput(state1, { () => "move Pluto 2, 2" })
    state2.message mustEqual "Pluto rolls 2, 2. Pluto moves from Start to 4"

    val state3 = GameEngine.processInput(state2, { () => "move Pippo 2, 3" })
    state3.message mustEqual "Pippo rolls 2, 3. Pippo moves from 6 to 11"

  }
}
