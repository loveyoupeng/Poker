package com.loveyoupeng.poker.model.entity.card



case class Card(val ranking: Ranking, val suit: Suit)(implicit val compareSuit: Boolean = true) extends Ordered[Card] {
  require(ranking != null, "Card's ranking can not be null")
  require(suit != null, "Card's suit can not be null")
  def compare(that: Card): Int = {
    if (compareSuit)
      if (ranking.compare(that.ranking) != 0)
        ranking.compare(that.ranking)
      else
        suit.compare(that.suit)
    else
      ranking.compare(that.ranking)
  }
}

object Card {
  def suits: List[Suit] = List(Spade, Heart, Club, Diamond)
  def rankings: List[Ranking] = List(Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace)
  object CardOrderingWithSuit extends Ordering[Card] {
    def compare(left: Card, right: Card): Int = {
      val rankingResult = left.ranking.compare(right.ranking)
      if (rankingResult == 0)
        left.suit.compare(right.suit)
      else
        rankingResult
    }
  }
  def makeAPack()(implicit compareSuit: Boolean = true): Pack = {
    Pack(for {
      suit <- suits;
      ranking <- rankings
    } yield Card(ranking, suit))
  }
}