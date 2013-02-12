package com.loveyoupeng.poker.model.entity.card

abstract sealed class Ranking(val value: Int) extends Ordered[Ranking] {
  def compare(that: Ranking) = value - that.value;
}
case object Ace extends Ranking(14);
case object Two extends Ranking(2);
case object Three extends Ranking(3);
case object Four extends Ranking(4);
case object Five extends Ranking(5);
case object Six extends Ranking(6);
case object Seven extends Ranking(7);
case object Eight extends Ranking(8);
case object Nine extends Ranking(9);
case object Ten extends Ranking(10);
case object Jack extends Ranking(11);
case object Queen extends Ranking(12);
case object King extends Ranking(13);

