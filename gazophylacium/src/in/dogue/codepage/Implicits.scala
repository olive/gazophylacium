package in.dogue.codepage

import in.dogue.codepage.data.{AugBool, AugAny, AugOption}

object Implicits {
  implicit def any2Aug[A](a:A) = new AugAny(a)
  implicit def opt2Aug[A](o:Option[A]) = new AugOption(o)
  implicit def bool2Aug(b:Boolean) = new AugBool(b)

  def id[T](t:T) = t
}
