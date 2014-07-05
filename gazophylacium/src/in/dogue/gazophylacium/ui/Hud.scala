package in.dogue.gazophylacium.ui

import in.dogue.antiqua.graphics.{TextFactory, Text, TileRenderer}
import com.deweyvm.gleany.graphics.Color

object Hud {
  def create = {
    val f = TextFactory(Color.Black, Color.White)
    Hud(f.create("7th Page"), f.create("???"), f)
  }
}

case class Hud(text:Text, coords:Text, f:TextFactory) {
  def withCoords(i:Int, j:Int) = {
    val s = "(%s,%s)".format(i, j)
    copy(coords=f.create(s))
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< text.draw(i, j) <+< coords.draw(i + 12, j)
  }
}
