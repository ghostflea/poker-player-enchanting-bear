package org.leanpoker.player

import com.google.gson.JsonParser
import org.scalatest._

class PlayerTest extends FunSpec with Matchers {
  
  import Player.Card

  val hold1 = List(Card(1, ""), Card(2, ""))
  val hold2 = List(Card(2, ""), Card(2, ""))
  val flop1 = List[Card]()
  val flop2 = List(Card(3, ""), Card(4, ""), Card(5, ""))
  val flop3 = List(Card(1, ""), Card(3, ""), Card(4, ""), Card(5, ""))
  val flop4 = List(Card(1, ""), Card(2, ""), Card(3, ""), Card(4, ""), Card(5, ""))
  val flop5 = List(Card(3, ""), Card(4, ""), Card(1, ""), Card(1, ""), Card(2, ""))
  
  it("check counting of pairs and three of a kind") {
    Player.findMultiples(hold1, flop1) should be ((0, 0))
    Player.findMultiples(hold2, flop1) should be ((1, 0))
    Player.findMultiples(hold1, flop2) should be ((0, 0))
    Player.findMultiples(hold1, flop3) should be ((1, 0))
    Player.findMultiples(hold1, flop4) should be ((1, 1))
    Player.findMultiples(hold1, flop5) should be ((2, 1))
    Player.findMultiples(hold2, flop4) should be ((2, 0))
    Player.findMultiples(hold2, flop5) should be ((2, 0))
    
  }
  
  it("check bet amounts") {
    Player.computeBet(4, 8, 1000, hold1, flop1) should be (4)
    Player.computeBet(1000, 1000, 1000, hold1, flop1) should be (0)
    Player.computeBet(1000, 1000, 1000, hold2, flop1) should be (1000)
  }

  describe("betPosition") {
    it("gives right position") {
      Player.getBetPosition(0, 5, 1) shouldBe Player.Early
      Player.getBetPosition(0, 5, 2) shouldBe Player.Mid
      Player.getBetPosition(0, 5, 3) shouldBe Player.Mid
      Player.getBetPosition(0, 5, 4) shouldBe Player.Late
      
      Player.getBetPosition(3, 5, 4) shouldBe Player.Early
      Player.getBetPosition(3, 5, 0) shouldBe Player.Mid
      
      Player.getBetPosition(3, 5, 2) shouldBe Player.Late
      Player.getBetPosition(3, 5, 3) shouldBe Player.Late
      Player.getBetPosition(3, 5, 0) shouldBe Player.Mid
    }
  }
}