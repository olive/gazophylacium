package in.dogue.antiqua.graphics


object TileRenderer {
  def create = TileRenderer(Map(), 0, 0)
}

case class TileRenderer(draws:Map[(Int,Int), Tile], originX:Int, originY:Int) {
  def at(i:Int, j:Int) = copy(originX = i, originY = j)
  def move(i:Int, j:Int) = copy(originX = originX + i, originY = originY + j)
  def movet(ij:(Int,Int)) = move(ij._1, ij._2)
  def <+(i:Int, j:Int, tile:Tile) = {
    val updated = draws.updated((i + originX, j + originY), tile)
    copy(draws = updated)
  }

  def `$>`(i:Int, j:Int, f:Tile => Tile):TileRenderer = {
    val t = draws.get((i, j))
    t.map(tile => {
      this <+ (i, j, f(tile))
    }).getOrElse(this)
  }

  def <+~(t:(Int,Int,Tile)) = this.<+ _ tupled t
  def <+?(t:Option[(Int,Int,Tile)]) = t.map {this <+~ _}.getOrElse(this)
  def <++(draws:Seq[(Int,Int,Tile)]) = {
    draws.foldLeft(this) { _ <+~ _}
  }

  def <+<(f:TileRenderer => TileRenderer) = {
    f(this)
  }

  def <+?<(f:Option[TileRenderer => TileRenderer]) = {
    f.foldLeft(this) { _ <+< _}
  }

  def <++<(draws:Seq[TileRenderer => TileRenderer]) = {
    draws.foldLeft(this) { _ <+< _}
  }

  def <--() = {
    TileRenderer(Map(), originX, originY)
  }

}
