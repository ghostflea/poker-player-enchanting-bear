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
  case object Pair extends BasicStartingHand
  case object LowerPair extends BasicStartingHand
  case object OneOver1Under extends BasicStartingHand
  case object TwoOver extends BasicStartingHand
  case object TwoUnder extends BasicStartingHand
  
  sealed trait BasicStartingComparison
  case object PairVs2Undercards extends BasicStartingComparison
  case object PairVsLowerPair extends BasicStartingComparison
  case object PairVs1Over1Under extends BasicStartingComparison
  case object TwoOverVs2Under extends BasicStartingComparison
  case object PairVs2Over extends BasicStartingComparison
  
  
  
  val headToHeadStartingProbabilities = Map(
    PairVs2Undercards -> 0.83,
    PairVsLowerPair -> 0.82,
    PairVs1Over1Under -> 0.71,
    TwoOverVs2Under -> 0.63,
    PairVs2Over -> 0.55
  )
  
  case class Hand(rank: Rank, firstValue: Int, secondValue: Int, kickers: List[Int])
  
  def handToBasicStartingHand(hand: Hand): Option[BasicStartingHand] = {
    hand.rank match {
      case Rank(0) => None
      case Rank(1) => None
      case _ => None
    }
  }
  
  def firstBetProbability(hand: Hand, numberOtherPlayers: Int): Double = {
    1.0
  }
}