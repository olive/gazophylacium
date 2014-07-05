package in.dogue.gazophylacium.world

import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.data.Point2i

object RoomMap {
  def load(roomCols:Int, roomRows:Int) = {
    val rows = 10
    val cols = 10
    RoomMap(Array2d.tabulate(rows, cols) { case (i, j) =>
      Room.createRandom(roomCols, roomRows, Point2i(i, j)).some
    })
  }
}

case class RoomMap(rooms:Array2d[Option[Room]]) {
  def apply(i:Int, j:Int):Option[Room] = rooms.getOption(i, j).flatten
}
