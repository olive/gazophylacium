package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.ui.MessageBox

object Readable {
  def create(i:Int, j:Int, t:Tile, xs:Vector[String]) = {
    val mb = MessageBox.createSpace(i, j, xs)
    Readable(i, j, t, mb)
  }
}

case class Readable(i:Int, j:Int, t:Tile, xs:MessageBox) {

  def isPos(p:Position) = p.x == i && p.y == j

  def standard:MessageBox = xs
  def paper:MessageBox = ???
  def update:Readable = this

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+ (i + this.i, j + this.j, t)
  }
}
