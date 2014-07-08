package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{TileRenderer, Animation, Tile}


case class Item(i:Int, j:Int, t:Animation, id:Int) {
  def withPosition(p:Int, q:Int) = copy(i=p, j=q)
  def update:Item = copy(t=t.update)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< t.draw(i, j)
  }

  override def equals(other:Any) = {
    other.isInstanceOf[Item] && other.asInstanceOf[Item].id == id
  }
}
