package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.Tile
import scala.util.Random

class ReadableFactory(read:Vector[String], page:Vector[String]) {
  def makeReadable(i:Int, j:Int, cols:Int, rows:Int, r:Random) = {
    Readable.create(i, j, cols, rows, read, page, r)
  }
}
