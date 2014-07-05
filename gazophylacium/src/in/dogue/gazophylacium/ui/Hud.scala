package in.dogue.gazophylacium.ui

import in.dogue.antiqua.graphics.{TextFactory, Text, TileRenderer}
import com.deweyvm.gleany.graphics.Color

object Hud {
  def create = {
    val f = TextFactory(Color.Black, Color.White)
    Hud(f.create("7th Page"))
  }
}

case class Hud(text:Text) {
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< text.draw(i, j)
  }
}
