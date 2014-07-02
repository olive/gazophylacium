package in.dogue.gazophylacium.graphics

object Rect {
  def create(cols:Int, rows:Int, tile:Tile) = {
    val tiles = for (i <- 0 until cols; j <- 0 until rows) yield {
      (i, j, tile)
    }
    Rect(cols, rows, tiles)
  }
}
case class Rect(cols:Int, rows:Int, tiles:Seq[(Int, Int, Tile)]) {
  def update = this
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <++ (tiles map { case (ii, jj, t) => (i+ii, j+jj, t)})
  }
}
