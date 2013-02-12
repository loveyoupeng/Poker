/**
 *
 */
package com.loveyoupeng.poker.model.entity.texasholdem

/**
 * @author loveyoupeng
 *
 */
object HandBuilder {
  def constructHands(community: Community, pocket: Pocket): List[Hand] = {
    onePocket(community, pocket) ++ twoPocket(community, pocket) :+ Hand(community.cards)
  }

  private def onePocket(community: Community, pocket: Pocket): List[Hand] = {
    for {
      pocket <- pocket.cards;
      ignore <- community.cards
    } yield Hand(community.cards.filter(_ != ignore) :+ pocket)
  }

  private def twoPocket(community: Community, pocket: Pocket): List[Hand] = {
    for {
      ignoreA <- community.cards;
      ignoreB <- community.cards.drop(community.cards.indexOf(ignoreA) + 1)
    } yield Hand(community.cards.filter(card => card != ignoreA && card != ignoreB) ++ pocket.cards)
  }
  def rankOf(hand: Hand): HandRanking = RoyalFlush
}