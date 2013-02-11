/**
 *
 */
package com.loveyoupeng.poker.model.entity.card


/**
 * @author loveyoupeng
 *
 */
abstract class DistinctCardSet(cards: List[Card], count: Int) extends CardSet(cards, count) {
  require(cards.distinct.size == count, "This card set(" + this.getClass().getSimpleName() + ") contains duplicated cards: " + cards.filter(cards.distinct.contains _))
}

