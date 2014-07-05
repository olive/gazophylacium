package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{Tile, TileRenderer, Animation}
import in.dogue.antiqua.data.Code
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.gazophylacium.data.Direction
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.data.{Point2d, Recti}

object Critter {
  def createSimple(i:Int, j:Int, roamSize:Int, r:Random) = {
    val rs = roamSize/2
    val t1 = Tile(Code.^, Color.Black, Color.Cyan)
    val t2 = Tile(Code.-, Color.Black, Color.Cyan)
    val anim = Animation.makeBlinker(15, Vector(t1, t2))
    Critter(Position.create(i, j), anim, Recti(i - rs, j - rs, roamSize, roamSize), r)
  }
}

case class Critter(p:Position, s:Animation, confine:Recti, r:Random) {
  def update:Critter = {
    val newPos = if (r.nextDouble > 0.9) {
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
