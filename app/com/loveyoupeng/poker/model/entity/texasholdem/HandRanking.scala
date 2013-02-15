/**
 *
 */
package com.loveyoupeng.poker.model.entity.texasholdem

import com.loveyoupeng.poker.model.entity.card.Nine
import com.loveyoupeng.poker.model.entity.card.Ranking
import com.loveyoupeng.poker.model.entity.card.Card
import com.loveyoupeng.poker.model.entity.texasholdem.HandRanking._
import com.loveyoupeng.poker.model.entity.card.Ace
import com.loveyoupeng.poker.model.entity.card.Five
import scala.language.implicitConversions

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

trait HandClassification {
  def fit(hand: Hand): Boolean
  def build(hand: Hand): HandRanking
}

object HandRanking {
  implicit def card2RankingInt(card: Card): Int = card.ranking.value
}

object RoyalFlush extends HandRanking(10) with HandClassification {
  def score = Int.MaxValue

  def fit(hand: Hand): Boolean = hand.cards.mapConserve(_.suit).distinct.size == 1 && hand.cards.filter(_ > 9).size == 5
  def build(hand: Hand): HandRanking = RoyalFlush
}

case class StraightFlush(private val top: Card) extends HandRanking(9) {
  def score = if (top.ranking != Ace) top else Five.value
}

object StraightFlush extends HandClassification {
  def fit(hand: Hand): Boolean = hand.cards.mapConserve(_.suit).distinct.size == 1 && (hand.cards.filter(_ < 14).size == 5 && hand.cards.last - hand.cards.head == 4 || hand.cards.last.ranking == Ace && hand.cards.reverse.tail.filter(_ < 6).size == 4)
  def build(hand: Hand): HandRanking = StraightFlush(hand.cards.last);
}

case class FourOfAKind(private val four: Card, private val kicker: Card) extends HandRanking(8) {
  def score = four * 100 + kicker
}

object FourOfAKind extends HandClassification {
  def fit(hand: Hand): Boolean = hand.cards.filter(_ == hand.cards.head).size == 4 || hand.cards.filter(_ == hand.cards.last).size == 4
  def build(hand: Hand): HandRanking = {
    if (hand.cards.filter(_ == hand.cards.head).size == 4)
      FourOfAKind(hand.cards.head, hand.cards.last)
    else
      FourOfAKind(hand.cards.last, hand.cards.head)
  }
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






 