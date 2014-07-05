package in.dogue.gazophylacium.data

import in.dogue.antiqua.Implicits._

object Direction {
  def fromDiff(dx:Int, dy:Int):Option[Direction] = {
    if (dx == Left.dx) {
      Left.some
    } else if (dx == Right.dx) {
      Right.some
    } else if (dy == Up.dy) {
      Up.some
    } else if (dy == Down.dy) {
      Down.some
    } else {
      None
    }
  }

  val All = Vector(Up, Down, Left, Right)
}
sealed trait Direction {
    val dx:Int
    val dy:Int
}
case object Up extends Direction {
  override val dx:Int = 0
  override val dy:Int = -1
}
case object Down extends Direction {
  override val dx:Int = 0
  override val dy:Int = 1
}
case object Left extends Direction {
  override val dx:Int = -1
  override val dy:Int = 0
}
case object Right extends Direction {
  override val dx:Int = 1
  override val dy:Int = 0
}
