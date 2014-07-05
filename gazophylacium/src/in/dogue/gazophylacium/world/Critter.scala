package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{Tile, TileRenderer, Animation}
import in.dogue.antiqua.data.Code
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.gazophylacium.data.Direction
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.data.{Point2d, Recti}

object Critter {
  def createSimple(i:Int, j:Int) = {
    val t1 = Tile(Code.^, Color.Black, Color.White)
    val t2 = Tile(Code.-, Color.Black, Color.White)
    val anim = Animation.create(Vector((15, t1), (15, t2)))
    Critter(Position.create(i, j), anim, Recti(10,10,20,20))
  }
}

case class Critter(p:Position, s:Animation, confine:Recti) {
  def update:Critter = {
    val newPos = if (Random.nextDouble > 0.9) {
      val r = new Random
      val d = Direction.All.randomR(r)
      val newPos = p --> d
      if (confine.contains(Point2d(newPos.x, newPos.y))) {
        newPos
      } else {
        p
      }
    } else {
      p
    }
    copy(p=newPos, s = s.update)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr `$>` (p.x, p.y, {t =>
      t.setCode(s.getCode).setFg(s.getFg)
    })
  }
}
