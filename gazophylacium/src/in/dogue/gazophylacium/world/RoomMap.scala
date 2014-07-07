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
    val rooms = Array2d.tabulate(cols, rows) { case (i, j) =>
      Room.createRandom(cols, rows, roomCols, roomRows, Point2i(i, j), r)
    }
    val infos = Array2d.tabulate(cols, rows) { case (i, j) =>
      RoomInfo(Seq())
    }
    RoomMap(rooms, infos)
  }
}

case class RoomMap(rooms:Array2d[Room], infos:Array2d[RoomInfo]) {
  def apply(i:Int, j:Int):Option[Room] = {
    for {
      r <- rooms.getOption(i, j)
      i <- infos.getOption(i, j)
    }  yield r.load(i)
  }

  def collect(s:Seq[Item]) = {
    val newInfos = infos.map { case (i, j, it) =>
      s.foldLeft(it) { case (acc, item) => acc.collect(item) }
    }
    copy(infos=newInfos)
  }
}
