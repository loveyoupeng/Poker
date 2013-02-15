/**
 *
 */
package com.loveyoupeng.poker.model.entity.texasholdem

import com.loveyoupeng.poker.model.entity.card.Nine
import com.loveyoupeng.poker.model.entity.card.Ranking
import com.loveyoupeng.poker.model.entity.card.Card

/**
 * @author loveyoupeng
 *
 */
sealed abstract class HandRanking(private val typeRanking: Int) extends Ordered[HandRanking] {
  def compare(that: HandRanking): Int = {
    if (typeRanking.compare(that.typeRanking) != 0)
      typeRanking.compare(that.typeRanking)
    else
      score.compare(that.score)
  }
  def score: Int
}

object HandRanking {
  implicit def card2RankingInt(card: Card): Int = card.ranking.value
}

import com.loveyoupeng.poker.model.entity.texasholdem.HandRanking._

object RoyalFlush extends HandRanking(10) {
  def score = Int.MaxValue
}

case class StraightFlush(private val top: Card) extends HandRanking(9) {
  def score = top
}
case class FourOfAKind(private val four: Card, private val kicker: Card) extends HandRanking(8) {
  def score = four * 100 + kicker
}
case class FullHouse(private val three: Card, private val pair: Card) extends HandRanking(7) {
  def score = three * 100 + pair
}

case class Flush(private val cards: List[Card]) extends HandRanking(6) {
  def score = cards.foldRight(0)((ranking, sum) => sum * 100 + ranking)
}

case class Straight(private val top: Card) extends HandRanking(5) {
  def score = top
}

case class ThreeOfAKind(private val three: Card, private val kickerB: Card, private val kickerS: Card) extends HandRanking(4) {
  require(kickerB > kickerS, "Kicker should be sorted")
  def score = three * 10000 + kickerB * 100 + kickerS
}

case class TwoPair(private val pairB: Card, private val pairS: Card, private val kicker: Card) extends HandRanking(3) {
  require(pairB > pairS, "pairs should be sorted")
  def score = pairB * 10000 + pairS * 100 + kicker
}

case class OnePair(private val pair: Card, private val kickers: List[Card]) extends HandRanking(2) {
  def score = pair * 1000000 + kickers.foldRight(0)((card, sum) => sum * 100 + card)
}

case class HighCard(private val cards: List[Card]) extends HandRanking(1) {
  def score = cards.foldRight(0)((card, sum) => sum * 100 + card)
}


//  }
//}
//
//object FourOfAKind extends HandType(8) {
//  def suite(hand: Hand) = {
//    hand.cards.map(_.ranking)
//      .foldLeft[Map[Ranking, Int]](Map[Ranking, Int]())((map: Map[Ranking, Int], ranking: Ranking) => { if (map.contains(ranking)) map updated (ranking, map(ranking) + 1) else map + (ranking -> 1); map })
//      .values.filter(_ == 4).size == 1
//  }
//}

 