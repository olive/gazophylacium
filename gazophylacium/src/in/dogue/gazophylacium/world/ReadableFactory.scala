package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.Tile

class ReadableFactory(t:Tile, read:Vector[String], page:Vector[String]) {
  def makeReadable(i:Int, j:Int) = Readable.create(i, j, t, read, page)
}
