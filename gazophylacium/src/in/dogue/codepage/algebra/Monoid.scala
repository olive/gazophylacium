package in.dogue.codepage.algebra

trait Monoid[M] {
  def zero:M
  def <+>(m1:M, m2:M):M
}
