package in.dogue.gazophylacium.graphics

import in.dogue.antiqua.graphics.{Tile, TileRenderer}
import com.deweyvm.gleany.data.Point2d
import in.dogue.antiqua.data.Code
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits
import Implicits._
import scala.math._
import in.dogue.antiqua.graphics.Tile
import scala.Some
import in.dogue.gazophylacium.audio.SoundManager

object Effect  {
  def create(cols:Int, rows:Int, epicenter:(Int,Int)) = {
    Effect(cols, rows, epicenter, 0, false)
  }
}

case class Effect(cols:Int, rows:Int, epicenter:(Int,Int), t:Int, played:Boolean) {
  val speed = 30f
  val radius = t/speed
  import scala.math.{abs, max}
  val inner = max(t - speed, 0)/speed
  val maxX = max(abs(cols - epicenter._1), abs(epicenter._1))
  val maxY = max(abs(rows - epicenter._2), abs(epicenter._2))
  val hypot = (maxX * maxX + maxY * maxY).sqrt
  println(hypot - radius)
  def update = {
    val newPlayed = if (t % 20 == 0 && hypot - radius > 0) {
      SoundManager.explode.play()
      false
    } else if (hypot - radius < 0 && !played){
      SoundManager.wave.play()
      true
    } else {
      if (t % 120 == 0) {
        SoundManager.bass.play()
      }
      true
    }
    copy(t=t+1, played=newPlayed)
  }

  def draw(tr:TileRenderer):TileRenderer = {

    val draws = for (i <- 0 until cols; j <- 0 until rows) yield {
      val d = (Point2d(epicenter._1, epicenter._2) - Point2d(i, j)).magnitude

      val dimAmt = min(1 + hypot - d - radius, 1)

      tr.draws.get((i, j)) match {
        case Some(tile) =>
          (i, j, tile.setFg(tile.fgColor.dim(dimAmt)).setBg(tile.bgColor.dim(dimAmt)))
        case None =>
          (i, j, Tile(Code.` `, Color.Black, Color.Black))

      }
    }

    tr <++ draws
  }

}
