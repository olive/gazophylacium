package in.dogue.antiqua

import in.dogue.antiqua.data._

object Implicits {
  implicit def any2Aug[A](a:A) = new AugAny(a)
  implicit def opt2Aug[A](o:Option[A]) = new AugOption(o)
  implicit def bool2Aug(b:Boolean) = new AugBool(b)
  implicit def indexedSeq2Aug[A](s:IndexedSeq[A]) = new AugIndexedSeq[A](s)
  implicit def num2Aug[A](a:A)(implicit n: Numeric[A]) = new AugNum(a)
  implicit def seq2AugPosSeq[A](seq:Seq[(Int,Int,A)]) = new AugPosSeq(seq)
  def id[T](t:T) = t

  def impossible = throw new Exception("Impossible")
}
