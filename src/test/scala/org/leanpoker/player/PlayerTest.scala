package org.leanpoker.player

import com.google.gson.JsonParser
import org.scalatest.{FunSpec, MustMatchers}

class PlayerTest extends FunSpec with MustMatchers {
  
  import Player.Card

  val hold1 = List(Card(1, ""), Card(2, ""))
  val hold2 = List(Card(2, ""), Card(2, ""))
  val flop1 = List[Card]()
  val flop2 = List(Card(3, ""), Card(4, ""), Card(5, ""))
  val flop3 = List(Card(1, ""), Card(3, ""), Card(4, ""), Card(5, ""))
  val flop4 = List(Card(1, ""), Card(2, ""), Card(3, ""), Card(4, ""), Card(5, ""))
  val flop5 = List(Card(3, ""), Card(4, ""), Card(1, ""), Card(1, ""), Card(2, ""))
  
  it("check counting of pairs and three of a kind") {
    Player.findMultiples(hold1, flop1) must be ((0, 0))
    Player.findMultiples(hold2, flop1) must be ((1, 0))
    Player.findMultiples(hold1, flop2) must be ((0, 0))
    Player.findMultiples(hold1, flop3) must be ((1, 0))
    Player.findMultiples(hold1, flop4) must be ((1, 1))
    Player.findMultiples(hold1, flop5) must be ((2, 1))
    Player.findMultiples(hold2, flop4) must be ((2, 0))
    Player.findMultiples(hold2, flop5) must be ((2, 0))
    
  }
  
  it("check bet amounts") {
    
  }
//  it("check basic bet amounts") {
//    Player.computeBet(100, 1000, ) must be (100)
//  }
//
//  it("check fold") {
//    Player.computeBet(0, 0, 200, 1000) must be (0)
//  }
//  
//  it("call all in on initial pair") {
//    Player.computeBet(0, 100, 1000, 1000) must be (1000)
//  }
}