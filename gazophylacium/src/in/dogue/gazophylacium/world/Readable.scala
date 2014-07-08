package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.antiqua.ui.MessageBox

object Readable {
  def create(i:Int, j:Int, t:Tile, read:Vector[String], page:Vector[String]) = {
    val readBox = MessageBox.createSpace(i, j, read)
    val pageBox = MessageBox.createSpace(i, j, page)
    Readable(i, j, t, readBox, pageBox)
  }
}

case class Readable(i:Int, j:Int, t:Tile, read:MessageBox, page:MessageBox) {

  def isPos(p:Position) = p.x == i && p.y == j

  def standard:MessageBox = read
  def paper:MessageBox = page
  def update:Readable = this

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+ (i + this.i, j + this.j, t)
  }
}
