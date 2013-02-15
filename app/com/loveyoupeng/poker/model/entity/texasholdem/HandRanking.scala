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
  def classifiers(): List[HandClassification] = {
    List(RoyalFlush, StraightFlush, FourOfAKind, FullHouse, Flush, Straight, ThreeOfAKind, TwoPair, OnePair, HighCard)
  }
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

object FullHouse extends HandClassification {
  def fit(hand: Hand): Boolean = (hand.cards.filter(_ == hand.cards.head).size == 2 &&
    hand.cards.filter(_ == hand.cards.last).size == 3) ||
    (hand.cards.filter(_ == hand.cards.head).size == 3 && hand.cards.filter(_ == hand.cards.last).size == 2)
  def build(hand: Hand): HandRanking = {
    if (hand.cards.filter(_ == hand.cards.head).size == 3)
      FullHouse(hand.cards.head, hand.cards.last)
    else
      FullHouse(hand.cards.last, hand.cards.head)
  }
}

case class Flush(private val cards: List[Card]) extends HandRanking(6) {
  def score = cards.foldRight(0)((ranking, sum) => sum * 100 + ranking)
}

object Flush extends HandClassification {
  def fit(hand: Hand): Boolean = hand.cards.mapConserve(_.suit).distinct.size == 1
  def build(hand: Hand): HandRanking = {
    Flush(hand.cards)
  }
}

case class Straight(private val top: Card) extends HandRanking(5) {
  def score = top
}

object Straight extends HandClassification {
  def fit(hand: Hand): Boolean = hand.cards.mapConserve(_.suit).distinct.size > 1 && hand.cards.last - hand.cards.head == 4
  def build(hand: Hand): HandRanking = {
    Straight(hand.cards.last)
  }
}

case class ThreeOfAKind(private val three: Card, private val kickerB: Card, private val kickerS: Card) extends HandRanking(4) {
  require(kickerB > kickerS, "Kicker should be sorted")
  def score = three * 10000 + kickerB * 100 + kickerS
}

object ThreeOfAKind extends HandClassification {
  def fit(hand: Hand): Boolean = hand.cards.mapConserve(_.ranking).distinct.size == 3 && hand.cards.groupBy(_.ranking).values.filter(_.size != 2).size == 3
  def build(hand: Hand): HandRanking = {
    val mapping = hand.cards.groupBy(_.ranking)
    val three = mapping.values.filter(_.size == 3).head(0)
    val kickers = mapping.values.filter(_.size == 1)
    if (kickers.head(0) > kickers.last(0))
      ThreeOfAKind(three, kickers.head(0), kickers.last(0))
    else
      ThreeOfAKind(three, kickers.last(0), kickers.head(0))

  }
}

case class TwoPair(private val pairB: Card, private val pairS: Card, private val kicker: Card) extends HandRanking(3) {
  require(pairB > pairS, "pairs should be sorted")
  def score = pairB * 10000 + pairS * 100 + kicker
}

object TwoPair extends HandClassification {
  def fit(hand: Hand): Boolean = hand.cards.mapConserve(_.ranking).distinct.size == 3 && hand.cards.groupBy(_.ranking).values.filter(_.size == 2).size == 2
  def build(hand: Hand): HandRanking = {
    val mapping = hand.cards.groupBy(_.ranking)
    val pairs = mapping.values.filter(_.size == 2)
    val kicker = mapping.values.filter(_.size == 1).head(0)
    if (pairs.head(0) > pairs.last(0))
      ThreeOfAKind(pairs.head(0), pairs.last(0), kicker)
    else
      ThreeOfAKind(pairs.last(0), pairs.head(0), kicker)

  }
}

case class OnePair(private val pair: Card, private val kickers: List[Card]) extends HandRanking(2) {
  def score = pair * 1000000 + kickers.foldRight(0)((card, sum) => sum * 100 + card)
}

object OnePair extends HandClassification {
  def fit(hand: Hand): Boolean = hand.cards.mapConserve(_.ranking).distinct.size == 4
  def build(hand: Hand): HandRanking = {
    val mapping = hand.cards.groupBy(_.ranking)
    val pairs = mapping.values.filter(_.size == 2).head(0)
    OnePair(pairs, hand.cards.filter(_.ranking != pairs.ranking))
  }
}

case class HighCard(private val cards: List[Card]) extends HandRanking(1) {
  def score = cards.foldRight(0)((card, sum) => sum * 100 + card)
}

object HighCard extends HandClassification {
  def fit(hand: Hand): Boolean = {
    val rankings = hand.cards.map(_.ranking).distinct
    hand.cards.mapConserve(_.suit).distinct.size != 1 && rankings.size == 5 && rankings.last.value - rankings.head.value > 4
  }
  def build(hand: Hand): HandRanking = {
    HighCard(hand.cards)
  }
}





 