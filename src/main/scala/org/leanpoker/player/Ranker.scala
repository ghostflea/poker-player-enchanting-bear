package org.leanpoker.player

case class Rank(value: Int) extends AnyVal

class Ranker {
  val rawProbabilities = Map(
    Rank(0) -> 0.174,
    Rank(1) -> 0.438,
    Rank(2) -> 0.235,
    Rank(3) -> 0.0483,
    Rank(4) -> 0.0462,
    Rank(5) -> 0.0303,
    Rank(6) -> 0.0206,
    Rank(7) -> 0.00168,
    Rank(8) -> 0.000311
  )
  
  sealed trait BasicStartingHand
  case object PairVs2Undercards extends BasicStartingHand
  case object PairVsLowerPair extends BasicStartingHand
  case object PairVs1Over1Under extends BasicStartingHand
  case object TwoOverVs2Under extends BasicStartingHand
  case object PairVs2Over extends BasicStartingHand
  
  
  val headToHeadStartingProbabilities = Map(
    
  )
  
  case class Hand(rank: Rank, firstValue: Int, secondValue: Int, kickers: List[Int])
  
  def firstBetProbability(hand: Hand, numberOtherPlayers: Int): Double = {
    0.0
  }
}