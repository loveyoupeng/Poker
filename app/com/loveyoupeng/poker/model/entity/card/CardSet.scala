/**
 *
 */
package com.loveyoupeng.poker.model.entity.card
 

/**
 * @author loveyoupeng
 *
 */
abstract class CardSet(val cards: List[Card],count:Int) {
  require(count>0,"Card set limitation for (" + this.getClass().getSimpleName() + ") should be greater than 0")
  require(cards != null && cards != Nil, "This card set(" + this.getClass().getSimpleName() + ") can not have empty cards")
  require(cards != Nil && cards.size == count, "This card set(" + this.getClass().getSimpleName() + ") should have " + count + " cards instead of " + cards.size)
}