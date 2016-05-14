package org.leanpoker.player

import play.api.libs.json._
import play.api.libs.json.Json._
import scala.util.Success
import scala.util.Try

object Player {

  val VERSION = "Looking at cards player"
  
  case class Card(rank: Int, suit: String)
  
  def convertRank(rank: String) = rank match {
    case "J" => 11
    case "Q" => 12
    case "K" => 13
    case "A" => 14
    case other => other.toInt
  }
  
  def convertCards(cards: JsArray): List[Card] = {
    val conv = cards.as[List[Map[String, String]]]
    conv.map { map => new Card(convertRank(map("rank")), map("suit")) }
  }
  
  def findMultiples(ours: List[Card], common: List[Card]): Int = {
    if (ours.size > 1 && ours(0).rank == ours(0).rank) common.count { y => ours.head.rank == y.rank } + 1
    else ours.map { x => common.count { y => x.rank == y.rank } }.max
  }
  
  def computeRaise(hole: List[Card], comm: List[Card]): Int = {
    val bestCount = findMultiples(hole, comm)
    if (bestCount > 2) 200
    else if (bestCount == 2) 100
    else if (bestCount == 1) 60
    else 0
  }
  
  def computeBet(round: Int, raise: Int, call: Int, stack: Int): Int = {
    if (raise == 0 && (round < 2 || round > 3) && call > stack / 8) 0
    else {
      val adjustedRaise =
        if (round < 2) raise
        else raise / round
      val wantToBet = call + raise
      if (wantToBet <= stack) wantToBet
      else if (call <= stack) stack
      else call
    }
  }

  def betRequest(request: JsValue) = {
    //request.getAsJsonObject.get
    println(request)
    val currentBuyIn = (request \ "current_buy_in").as[JsNumber].value.toInt
    val inAction = (request \ "in_action").as[JsNumber].value.toInt
    
    val players = (request \ "players").as[JsArray]
    val activePlayerBet = (players(inAction) \ "bet").as[JsNumber].value.toInt
    
    val callAmount = currentBuyIn - activePlayerBet
    val minimumRaise = (request \ "minimum_raise").as[JsNumber].value.toInt
    
    val stack = (players(inAction) \ "stack").as[JsNumber].value.toInt
    
    val commCards = convertCards((request \ "community_cards").as[JsArray])
    val ourCards = convertCards((players(inAction) \ "hole_cards").as[JsArray])
    val cardRaise = computeRaise(ourCards, commCards)
    computeBet(commCards.length, cardRaise, callAmount, stack)
  }

  def showdown(game: JsValue) {

  }
  
  def main(args: Array[String]): Unit = {
    val betReq = """{ "bet_index" : 4,
  "big_blind" : 4,
  "community_cards" : [ { "rank" : "K",
        "suit" : "diamonds"
      },
      { "rank" : "K",
        "suit" : "clubs"
      },
      { "rank" : "3",
        "suit" : "clubs"
      },
      { "rank" : "A",
        "suit" : "spades"
      },
      { "rank" : "2",
        "suit" : "diamonds"
      }
    ],
  "current_buy_in" : 4,
  "dealer" : 4,
  "game_id" : "57365c31f3376a0003000048",
  "in_action" : 4,
  "minimum_raise" : 4,
  "orbits" : 1,
  "players" : [ { "bet" : 2,
        "id" : 0,
        "name" : "CardCounter",
        "stack" : 996,
        "status" : "folded",
        "version" : "Default Haskell folding player"
      },
      { "bet" : 0,
        "id" : 1,
        "name" : "AllOfTheParens",
        "stack" : 0,
        "status" : "out",
        "version" : "Clojure-y pairs"
      },
      { "bet" : 4,
        "id" : 2,
        "name" : "HaskellRulz",
        "stack" : 998,
        "status" : "active",
        "version" : "Default Haskell folding player"
      },
      { "bet" : 0,
        "id" : 3,
        "name" : "Elixir",
        "stack" : 988,
        "status" : "folded",
        "version" : "0.0.1"
      },
      { "bet" : 4,
        "hole_cards" : [ { "rank" : "2",
              "suit" : "spades"
            },
            { "rank" : "2",
              "suit" : "hearts"
            }
          ],
        "id" : 4,
        "name" : "Enchanting Bear",
        "stack" : 2008,
        "status" : "active",
        "version" : "Default Scala folding player"
      }
    ],
  "pot" : 10,
  "round" : 7,
  "small_blind" : 2,
  "tournament_id" : "572efe4efc0f49000300002b"
}"""
    val parsed = parse(betReq)
    val communityCards = (parsed \ "community_cards").as[JsArray]
    println(communityCards)
    val commCards = convertCards(communityCards)
    println(commCards)
    val players = (parsed \ "players")
    val inAction = (parsed \ "in_action").as[JsNumber].value.toInt
    val holeCards = (players(inAction) \ "hole_cards").as[JsArray]
    val ourCards = convertCards(holeCards)
    println(ourCards)
    println(findMultiples(ourCards, commCards))
    val stack = (players(inAction) \ "stack").as[JsNumber].value.toInt
    println(stack)
  }
}
