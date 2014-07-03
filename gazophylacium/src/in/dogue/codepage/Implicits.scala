package in.dogue.codepage

import in.dogue.codepage.data.{AugAny, AugOption}

object Implicits {
  implicit def any2Aug[A](a:A) = new AugAny(a)
  implicit def opt2Aug[A](o:Option[A]) = new AugOption(o)
}
