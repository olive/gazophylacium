package in.dogue.codepage.graphics

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
