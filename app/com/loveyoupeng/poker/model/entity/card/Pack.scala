/**
 *
 */
package com.loveyoupeng.poker.model.entity.card
import scala.collection.immutable.List
import scala.util.Random 
/**
 * @author loveyoupeng
 *
 */
case class Pack(private var pack: List[Card]) extends DistinctCardSet(pack,52) {
  def shuffle: List[Card] = {
    pack = Random.shuffle(pack)
    pack
  }
}