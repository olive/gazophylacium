package in.dogue.antiqua.data

class AugPosSeq[T](s:Seq[(Int,Int,T)]) {
  def smap(f:T => T) = s.map{ case (i, j, t) => (i, j, f(t))}

}
