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
case class Community(val flop:Flop,val turn:Card,val river:Card) extends DistinctCardSet(flop.cards ++ (turn :: river :: Nil),5)