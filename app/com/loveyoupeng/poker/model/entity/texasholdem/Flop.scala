/**
 *
 */
package com.loveyoupeng.poker.model.entity.texasholdem

import com.loveyoupeng.poker.model.entity.card.DistinctCardSet

import com.loveyoupeng.poker.model.entity.card.Card

/**
 * @author loveyoupeng
 *
 */
case class Flop(val first:Card,val second:Card,val third:Card) extends DistinctCardSet(List(first,second,third),3)