package com.loveyoupeng.poker.model.entity.texasholdem

import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.matcher.Matchers
import scala.util.Random
import com.loveyoupeng.poker.model.entity.card.Suit
import com.loveyoupeng.poker.model.entity.card.Spade
import com.loveyoupeng.poker.model.entity.card.Heart
import com.loveyoupeng.poker.model.entity.card.Diamond
import com.loveyoupeng.poker.model.entity.card.Club
import com.loveyoupeng.poker.model.entity.card.CardSet
import com.loveyoupeng.poker.model.entity.card.Ten
import com.loveyoupeng.poker.model.entity.card.Ranking
import com.loveyoupeng.poker.model.entity.card.Queen
import com.loveyoupeng.poker.model.entity.card.King
import com.loveyoupeng.poker.model.entity.card.Jack
import com.loveyoupeng.poker.model.entity.card.Ace
import com.loveyoupeng.poker.model.entity.card.DistinctCardSet
import com.loveyoupeng.poker.model.entity.card.Card
/**
 * @author loveyoupeng
 *
 */
@RunWith(classOf[JUnitRunner])
class HandSpec extends Specification {
   
  implicit val compareSuit: Boolean = false
   
  "The Pocket" should {
    "contains 2 cards" in {
      Pocket(Card(Ace, Spade), Card(King, Spade)).cards must have size (2)
    }
  }

  "The Flop" should {
    "contains 3 cards" in {
      Flop(Card(Queen, Spade), Card(Jack, Spade), Card(Ten, Spade)).cards must have size (3)
    }
  }

  "The Community" should {
    "contains 1 Flop, 1 turn and 1 river and in total 5 cards" in {
      val flop = Flop(Card(Queen, Spade), Card(Jack, Spade), Card(Ten, Spade))
      val turn = Card(King, Spade)
      val river = Card(Ace, Spade)
      val community = Community(flop, turn, river)
      community.flop must equalTo(flop)
      community.turn must equalTo(turn)
      community.river must equalTo(river)
    }
  }

  "The Hand" should {
    "contains 5 cards" in {
      Hand(List(Card(Ace, Spade), Card(King, Spade), Card(Queen, Spade), Card(Jack, Spade), Card(Ten, Spade))).cards must have size (5)
      Hand(List(Card(Ace, Spade), Card(King, Spade), Card(Queen, Spade))).cards must throwA[Exception]
    }

    "cards shold be sorted" in {
      val sorted = List(Card(Ten, Spade), Card(King, Diamond), Card(King, Club), Card(Ace, Heart), Card(Ace, Spade))
      val hand = Hand(sorted.reverse)
      hand.cards must equalTo(sorted)
    }
  }
}