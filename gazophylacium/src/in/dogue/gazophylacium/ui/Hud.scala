package in.dogue.gazophylacium.ui

import in.dogue.antiqua.graphics._
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code
import in.dogue.antiqua.graphics.Text
import in.dogue.gazophylacium.world.Item
import scala.collection.immutable.IndexedSeq
import scala.util.Random
import in.dogue.antiqua.Implicits
import Implicits._

object Hud {
  def create(width:Int, height:Int, r:Random) = {
    val f = TextFactory(Color.Black, Color.White)
    def p1 = Color.Pink.dim(7 + r.nextDouble)
    val p2 = Color.Pink.dim(6 + r.nextDouble)
    def makeTile(r:Random) = {
      val code = Vector(Code.?, Code.!, Code.`¿`, Code.`¡`).randomR(r)
      val bg = p1
      val fg = p2
      Tile(code, bg, fg)
    }
    val bar = (0 until width) map { i =>
      (i, height - 1, Tile(Code.underscore, p1, Color.White))
    }
    val rect = Rect.createTextured(width, height, makeTile, r)
    Hud(height, f.create("7th Page"), f.create("???"), Seq(), bar, rect, f)
  }
}

case class Hud(height:Int, text:Text, coords:Text, s:Seq[Item], bar:Seq[(Int,Int,Tile)], r:Rect, f:TextFactory) {
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
    tr <+< r.draw(0,0) <+< text.drawFg(0, 0) <+< coords.drawFg(0 + 12, 0) <+< drawItems(24, 1) <++ bar
  }
}
