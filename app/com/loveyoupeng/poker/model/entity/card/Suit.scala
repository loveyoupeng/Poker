/**
 *
 */
package com.loveyoupeng.poker.model.entity.card

/**
 * @author loveyoupeng
 *
 */
abstract sealed class Suit(private val value:Int) extends Ordered[Suit]{
  def compare(that: Suit) = value - that.value;
}
case object Spade extends Suit(4)
case object Heart extends Suit(3)
case object Club extends Suit(2)
case object Diamond extends Suit(1) 

