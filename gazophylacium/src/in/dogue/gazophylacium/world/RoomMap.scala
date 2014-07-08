package in.dogue.gazophylacium.world

import in.dogue.antiqua.data.{Code, Array2d}
import com.deweyvm.gleany.data.Point2i
import scala.util.Random
import com.deweyvm.gleany.graphics.Color

object RoomMap {
  def load(roomCols:Int, roomRows:Int) = {
    val cols = 5
    val rows = 5

    val r = new Random(0)
    val specs = RoomSpec.makeSpecs(cols, rows)
    val indices: Map[(Int, Int), ItemFactory] = r.shuffle(for (i <- 0 until cols; j <- 0 until rows) yield (i, j)).take(6).zip(createItems).toMap
    val rooms = specs.map { case (i, j, s) =>
      val items = indices.get((i, j)).map { t => Seq(t) }.getOrElse(Seq())
      s.createRoom(cols, rows, roomCols, roomRows, Point2i(i, j), items, r)
    }
    val infos = Array2d.tabulate(cols, rows) { case (i, j) =>
      RoomInfo(Seq())
    }
    RoomMap(rooms, infos)
  }

  def createItems = {

    Vector(
      ItemFactory(Code.¶, Color.Cyan, Vector(""), Vector("")),
      ItemFactory(Code.┼, Color.Tan, Vector(""), Vector("")),
      ItemFactory(Code.÷, Color.Red, Vector(""), Vector("")),
      ItemFactory(Code.`¢`, Color.Grey, Vector(""), Vector("")),
      ItemFactory(Code.τ, Color.White, Vector(""), Vector("")),
      ItemFactory(Code.§, Color.Yellow, Vector(""), Vector(""))

    )
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
