/**
 *
 */
package com.loveyoupeng.poker.model.entity.texasholdem

import com.loveyoupeng.poker.model.entity.card.Nine

/**
 * @author loveyoupeng
 *
 */
abstract class HandRanking

sealed abstract class HandType(private val value: Int) extends Ordered[HandType] {
  def compare(that: HandType): Int = value.compare(that.value)
  def suite(hand: Hand): Boolean
}

object RoyalFlush extends HandType(10) {
  def suite(hand: Hand) = {
    hand.cards.mapConserve(_.suit).distinct.size == 1 && hand.cards.filter(_.ranking > Nine).size == 5
  }
}

 