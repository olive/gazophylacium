package in.dogue.gazophylacium.ui

import in.dogue.antiqua.graphics._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code
import in.dogue.antiqua.graphics.Text
import in.dogue.gazophylacium.world.Item
import scala.collection.immutable.IndexedSeq

object Hud {
  def create(width:Int, height:Int) = {
    val f = TextFactory(Color.Black, Color.White)
    val r = Rect.create(width, height, Tile(Code.` `, Color.Black, Color.Black))
    Hud(height, f.create("7th Page"), f.create("???"), Seq(), r, f)
  }
}

case class Hud(height:Int, text:Text, coords:Text, s:Seq[Item], r:Rect, f:TextFactory) {
  def withCoords(i:Int, j:Int) = {
    val s = "(%s,%s)".format(i, j)
    copy(coords=f.create(s))
  }
  def withInventory(s:Seq[Item]) = {
    copy(s=s)
  }

  def drawItems(p:Int, q:Int)(tr:TileRenderer):TileRenderer = {
    val rows = 2
    val cols = 3
    val draws = for (j <- 0 until rows ; i <- 0 until cols) yield {
      val k = i + j*cols
      val f = s.find{it => it.id == k}
      f.map {_.withPosition(i + p, j + q).draw _}
    }
    tr <++< draws.flatten
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< r.draw(0,0) <+< text.draw(0, 0) <+< coords.draw(0 + 12, 0) <+< drawItems(24, 1)
  }
}
