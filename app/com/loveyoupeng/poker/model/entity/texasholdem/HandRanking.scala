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

object RoyalFlush extends HandRanking(10) {
  def score = Int.MaxValue
}

case class StraightFlush(private val top: Card) extends HandRanking(9) {
  def score = top.ranking.value
}
case class FourOfAKind(private val four: Card, private val kicker: Card) extends HandRanking(8) {
  def score = four.ranking.value * 100 + kicker.ranking.value
}
case class FullHouse(private val three: Card, private val pair: Card) extends HandRanking(7) {
  def score = three.ranking.value * 100 + pair.ranking.value
}

case class Flush(private val hand: Hand) extends HandRanking(6) {
  def score = hand.cards.map(_.ranking.value).foldRight(0)((ranking, sum) => sum * 100 + ranking)
}

case class Straight(private val top: Card) extends HandRanking(5) {
  def score = top.ranking.value
}

case class ThreeOfAKind(private val three: Card, private val kickerB: Card, private val kickerS: Card) extends HandRanking(4) {
  require(kickerB > kickerS, "Kicker should be sorted")
  def score = three.ranking.value * 10000 + kickerB.ranking.value * 100 + kickerS.ranking.value
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

 