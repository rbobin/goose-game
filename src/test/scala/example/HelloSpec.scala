package example

import org.scalacheck.Prop.BooleanOperators
import org.specs2.{Specification, _}

import scala.collection.immutable._

class HelloSpec extends Specification with ScalaCheck {
  def is = s2"""

  "add player" command:
    adds a player if there is no such name $e1
    does not add a player if there is such name $e2

  """

  def e1 =
    prop { (players: Set[String], player: String) =>
      !players.contains(player) ==> {
        val state = GameState(ListMap.empty ++ players.toList.map(n => n -> 0))
        val changedState = GameEngine.addPlayer(player, state)

        (changedState.players.keySet mustEqual players + player) and
          (changedState.message mustEqual s"players: ${(players.toList :+ player).mkString(", ")}")
      }
    }

  def e2 =
    prop { players: List[String] =>
      players.nonEmpty ==> {
        val state = GameState(ListMap.empty ++ players.map(n => n -> 0))
        val changedState = GameEngine.addPlayer(players.head, state)

        (changedState.players.keySet mustEqual players.toSet) and
          (changedState.message mustEqual s"${players.head}: already existing player")
      }
    }
}
