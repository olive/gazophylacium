package in.dogue.antiqua.data

import scala.util.Random

class AugIndexedSeq[T](s:IndexedSeq[T]) {
  def random():T = s(Random.nextInt(s.length))
  def randomR(r:Random) = s(r.nextInt(s.length))
}
