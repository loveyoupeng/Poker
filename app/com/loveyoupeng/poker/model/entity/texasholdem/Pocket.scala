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
case class Pocket(val first:Card,val second:Card) extends DistinctCardSet(List(first,second),2)