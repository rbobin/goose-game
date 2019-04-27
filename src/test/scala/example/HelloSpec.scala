package example

import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties
import org.scalatest._


class HelloSpec extends Properties("addPlayer") with Matchers {

  property("existing-players") = forAll { (players: Set[String], player: String) =>
    (players.nonEmpty && (!players.contains(player))) ==> {
      val state = GameState(players.toList)
      val changedState = GameEngine.addPlayer(player, state)

      changedState.players == player :: players.toList &&
      changedState.message == s"players: ${(player :: players.toList).mkString(", ")}"
    }
  }

  property("no-players") = forAll { player: String =>
    val state = GameState()
    val changedState = GameEngine.addPlayer(player, state)

    changedState.players == List(player) &&
    changedState.message == s"players: $player"
  }

  property("duplicate-player") = forAll { players: List[String] =>
    players.nonEmpty ==> {
      val state = GameState(players)
      val changedState = GameEngine.addPlayer(players.head, state)

      changedState.players == players &&
      changedState.message == s"${players.head}: already existing player"
    }
  }
}
