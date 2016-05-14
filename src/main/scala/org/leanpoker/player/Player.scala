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
  
  /**
   * Given out cards and common cards, returns best results combining our cards with common cards in the form of a
   * pair. Each value in the pair is the number of cards in the set found, minus one - so 1 for a pair, 2 for
   * three-of-a-kind, and 3 for four-of-a-kind. The highest value is always the first in the pair. A full house
   * using cards in our hand for both sets would be (2, 1).
   */
  def findMultiples(ours: List[Card], common: List[Card]): (Int, Int) = {
    if (ours.size > 1 && ours(0).rank == ours(1).rank) {
      val count = common.count { y => ours.head.rank == y.rank } + 1
      (count, 0)
    } else {
      val counts = ours.map { x => common.count { y => x.rank == y.rank } }.sorted
      (counts(1), counts(0))
    }
  }

  val earlyHands = List(
    List(Card(14, ""), Card(14, "")),
    List(Card(13, ""), Card(13, "")),
    List(Card(12, ""), Card(12, "")),
    List(Card(11, ""), Card(11, ""))
  )
  
  val earlyHandsSuited = List(
    List(Card(14, "S"), Card(13, "S")),
    List(Card(14, "S"), Card(12, "S"))
  )
  
  val midHands = List(
    List(Card(14, ""), Card(12, "")),
    List(Card(10, ""), Card(10, "")),
    List(Card(9, ""), Card(9, ""))
  )
  
  val midHandsSuited = List(
    List(Card(14, "S"), Card(11, "S")),
    List(Card(13, "S"), Card(12, "S"))
  )
  
  val lateHands = List(
    List(Card(14, "S"), Card(10, "S")),
    List(Card(14, ""), Card(11, "")),
    List(Card(8, ""), Card(8, ""))
  )
  
  val lateHandsSuited = List(
    List(Card(14, "S"), Card(10, "S")),
    List(Card(13, "S"), Card(11, "S"))
  )
  
  sealed trait BetPosition
  case object Early extends BetPosition
  case object Mid extends BetPosition
  case object Late extends BetPosition
  
  def inPositionPlay(position: BetPosition, card1: Card, card2: Card): Boolean = {
     position match {
      case Early => inPositionPlaySub(earlyHands, earlyHandsSuited)(card1, card2)
      case Mid => inPositionPlaySub(midHands, midHandsSuited)(card1, card2)
      case Late => inPositionPlaySub(lateHands, lateHandsSuited)(card1, card2)
    }
  }
  
  def inPositionPlaySub(nonSuited: List[List[Card]], suited: List[List[Card]])(card1: Card, card2: Card): Boolean = {
    val handSuited = card1.suit == card2.suit
    
    def inHands(hands: List[List[Card]]) =
      hands.exists(pair => pair.filter(p => p.rank == card1.rank).filter(s => s.rank == card2.rank).length == 0)
    
    inHands(nonSuited) || (handSuited && inHands(suited))
  }
  
  def computeFirstRaise(hole: (Card, Card), ourBet: Integer, pot: Integer, position: BetPosition): Int = {
    val goodHand = inPositionPlay(position, hole._1, hole._2)
    
    if (goodHand) 100
    else 0
  }
  
  def getBetPosition(dealer: Int, playerCount: Int, ourIndex: Int): BetPosition = {
    //(dealer+1)%(players.length)
    
    val firstPlayer = (dealer+1) % playerCount
    val ourPosition = 
      if (ourIndex < firstPlayer)
        (ourIndex - firstPlayer) + playerCount
      else
        ourIndex - firstPlayer
        
    val segment = playerCount.toDouble / 3.0
    
    val early = Math.floor(segment)
    val mid = early + Math.ceil(segment)
    val late = mid + Math.ceil(segment)
    
    if (ourPosition < early)
      Early
    else if (ourPosition < mid)
      Mid
    else
      Late
  }
  
  def computeBet(call: Int, bigBlind: Int, minRaise: Int, inFor: Int, pot: Int, stack: Int, hole: List[Card], comm: List[Card]): Int = {
    val (mult1, mult2) = findMultiples(hole, comm)
    val rank = comm.size match {
      case 0 => if (mult1 > 0) 100 else 0
      case 3 | 4 =>
        (mult1, mult2) match {
          case (1, 0) => 20
          case (1, 1) => 50
          case (2, 0) => 100
          case (2, 1) => 200
          case (3, 0) => 1000
          case _ => 0
        }
      case _ =>
        (mult1, mult2) match {
          case (1, 0) => 0
          case (1, 1) => 25
          case (2, 0) => 50
          case (2, 1) => 100
          case (3, 0) => 500
          case _ => 0
        }
    }
    if (rank == 0) {
      if (call < stack / 20 && comm.size < 2) call
      else 0
    } else {
      val baseBet = (pot / 1000 + 1) * rank
      val threshold = math.max(baseBet * 4, pot / 8)
      val atRisk = math.min(call, stack)
      if (comm.size > 1 && atRisk > threshold) 0
      else math.max(math.max(call, pot / 2), baseBet)
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
    val pot = (request \ "pot").as[JsNumber].value.toInt
    val bigBlind = (request \ "big_blind").as[JsNumber].value.toInt
    val minRaise = (request \ "minimum_raise").as[JsNumber].value.toInt
    
    val commCards = convertCards((request \ "community_cards").as[JsArray])
    val ourCards = convertCards((players(inAction) \ "hole_cards").as[JsArray])
    val bet = computeBet(callAmount, bigBlind, minRaise, activePlayerBet, pot, stack, ourCards, commCards)
    println(s"computeBet($callAmount, $stack, $ourCards, $commCards) = $bet")
    bet
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
