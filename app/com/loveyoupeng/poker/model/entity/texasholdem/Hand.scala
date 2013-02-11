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
case class Hand(chosenCards:List[Card]) 
	extends DistinctCardSet(chosenCards.sorted(Card.CardOrderingWithSuit),5)