package in.dogue.gazophylacium.world

import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.data.Point2i
import scala.util.Random

object RoomMap {
  def load(roomCols:Int, roomRows:Int) = {
    val cols = 3
    val rows = 3

    val r = new Random(0)
    RoomMap(Array2d.tabulate(cols, rows) { case (i, j) =>
      Room.createRandom(cols, rows, roomCols, roomRows, Point2i(i, j), r).some
    })
  }
}

case class RoomMap(rooms:Array2d[Option[Room]]) {
  def apply(i:Int, j:Int):Option[Room] = rooms.getOption(i, j).flatten
}
