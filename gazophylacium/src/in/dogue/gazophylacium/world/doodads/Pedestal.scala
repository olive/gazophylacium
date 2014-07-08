package in.dogue.gazophylacium.world.doodads

import in.dogue.antiqua.graphics.{Tile, Animation, TileRenderer}
import com.deweyvm.gleany.data.{Point2d, Recti}
import in.dogue.antiqua.data.{Array2d, Code}
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits
import Implicits._
object Pedestal {
  def create = {
    val bgc = Color.Black
    val fgc = Color.White
    val row1 = Vector(Code.┌, Code.╥, Code.┐)
    val row2 = Vector(Code.╞, Code.`@`, Code.╡)
    val row3 = Vector(Code.│, Code.` `, Code.│)
    val row4 = Vector(Code.╘, Code.` `, Code.╛)
    val rows = Vector(row1, row2, row3, row4)
    val sprites = Array2d.tabulate(3, 4) { case (i, j) =>
      val code = rows(j)(i)
      if (code == Code.` `) {
        None
      } else {
        val tile = Tile(code, bgc, fgc)
        Animation.singleton(tile).some
      }
    }

    val flat = sprites.flatten.map { case (i, j, t) =>
      t match {
        case None => None
        case Some(tt) => (i, j, tt).some
      }

    }
    val bg = Vector(
      (1, 3, Animation.singleton(Tile(Code.`=`, bgc, fgc)))
    )
    Pedestal(flat.flatten, bg, 0)
  }
}
case class Pedestal(fg:Seq[(Int,Int,Animation)], bg:Seq[(Int,Int,Animation)], t:Int) {
  final val width = 3
  final val height = 4
  def update = copy(t=t+1)
  def getRect(i:Int, j:Int) = {
    Recti(i, j, width, height)
  }

  private def goodRect(i:Int, j:Int) = {
    Recti(i+1, j+2, 1, 2)
  }


  def isSolid(i:Int, j:Int)(p:Int, q:Int) = {
    val pt = Point2d(p, q)
    getRect(i, j).contains(pt) && !goodRect(i, j).contains(pt)
  }

  private def colorPulse(t:Int):Color = {
    val dim = Math.abs(Math.sin(t / 120f)*2)
    Color.Yellow.dim(dim.toFloat)
  }

  def drawBg(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <## (bg |+| (i, j))
  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    //tr <## (fg |+| (i, j))
    tr <++< fg.map { case (p, q, a) =>
      a.drawWithFg(colorPulse(t), i + p, j + q) _
    }
  }

  def toDoodad(i:Int, j:Int) = {
    Doodad[Pedestal](i, j, _.draw, _.drawBg, _.getRect, _.isSolid, _.update, this)
  }
}
