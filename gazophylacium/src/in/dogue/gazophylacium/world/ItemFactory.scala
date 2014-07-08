package in.dogue.gazophylacium.world

import in.dogue.antiqua.data.Code
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.graphics.{Animation, Tile}

case class ItemFactory(code:Code, color:Color, read:Vector[String], page:Vector[String]) {
  def makeItem(i:Int, j:Int) = {
    val tile = Tile(code, Color.Black, color)
    Item.create(i, j, Animation.singleton(tile))
  }

  def makeReadable(i:Int, j:Int) = {
    Readable.create(i, j, read, page)
  }
}
