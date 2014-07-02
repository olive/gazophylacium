package in.dogue.gazophylacium.graphics

import in.dogue.gazophylacium.data.Code
import com.deweyvm.gleany.graphics.Color





case class Text(tiles:Vector[Tile], f:TextFactory)  {
  val length = tiles.length
  def append(s:String) = {
    val other = f.create(s)
    concat(other)
  }

  private def concat(t:Text) = Text(tiles ++ t.tiles, f)

  def draw(i:Int, j:Int)(r:TileRenderer):TileRenderer = {
    r <++ tiles.zipWithIndex.map{case (t, k) => (i + k, j, t)}
  }
}
