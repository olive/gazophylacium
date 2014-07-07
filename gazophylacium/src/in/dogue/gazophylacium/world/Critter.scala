package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{Rect, Tile, TileRenderer, Animation}
import in.dogue.antiqua.data.Code
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.gazophylacium.data.Direction
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.data.{Point2d, Recti}

object Critter {
  private def create(i:Int, j:Int, anim:Seq[(Int,Int,Animation)], roamSize:Int, prob:Double, r:Random) = {
    val rs = roamSize/2
    val rect = Recti(i - rs, j - rs, roamSize, roamSize)
    Critter(Position.create(i, j), anim, rect, prob, r)

  }

  private def simpleAnimal(fg:Color, c1:Code, c2:Code, speed:Int)(i:Int, j:Int, roamSize:Int, prob:Double, r:Random) = {
    val t1 = Tile(c1, Color.Black/*unused*/, fg)
    val t2 = Tile(c2, Color.Black/*unused*/, fg)
    val anim = Animation.makeBlinker(speed, Vector(t1, t2))
    create(i, j, Seq((0,0,anim)), roamSize, prob, r)
  }

  def createRandom(i:Int, j:Int, roamSize:Int, r:Random) = {
    val fs = Vector(createBird _,
                    createSnake _,
                    createDragonfly _,
                    createFrog _,
                    createBug _,
                    createLizard _)
    val f = fs.randomR(r)
    f(i, j, roamSize, r)
  }

  def createBug(i:Int, j:Int, roamSize:Int, r:Random) = {
    simpleAnimal(Color.Black, Code.underscore, Code.`.`, 60)(i, j, roamSize, 0.99, r)
  }

  def createFrog(i:Int, j:Int, roamSize:Int, r:Random) = {
    simpleAnimal(Color.Brown, Code.ö, Code.Ö_u, 15)(i, j, roamSize, 0.99, r)
  }

  def createDragonfly(i:Int, j:Int, roamSize:Int, r:Random) = {
    simpleAnimal(Color.Red, Code.æ, Code.`¡`, 3)(i, j, roamSize, 0.8, r)
  }

  def createBird(i:Int, j:Int, roamSize:Int, r:Random) = {
    simpleAnimal(Color.Cyan, Code.^, Code.-, 15)(i, j, roamSize, 0.9, r)
  }

  def createSnake(i:Int, j:Int, roamSize:Int, r:Random) = {
    simpleAnimal(Color.Green, Code.~, Code.-, 15)(i, j, roamSize, 0.99, r)
  }

  def createLizard(i:Int, j:Int, roamSize:Int, r:Random) = {
    val bg = Color.Black
    val fg = Color.LightGreen
    val leg0_0 = Tile(Code.`[`, bg, fg)
    val leg0_1 = Tile(Code.│, bg, fg)
    val leg0_2 = Tile(Code.`]`, bg, fg)
    val legAnim0 = Animation.create(Vector(
      (23, leg0_0),
      (61, leg0_1),
      (17, leg0_2)
    ))
    val legAnim1 = Animation.create(Vector(
      (13, leg0_0),
      (61, leg0_1),
      (15, leg0_2)
    ))
    val trunk = Animation.create(Vector((1, Tile(Code.`=`, bg, fg))))
    val head = Animation.create(Vector((1, Tile(Code.`º`, bg, fg))))
    val body = Seq(
      (0, 0, legAnim0),
      (1, 0, trunk),
      (2, 0, legAnim1),
      (3, 0, head)

    )
    Critter.create(i, j, body, roamSize, 0.99, r)
  }

}

case class Critter(p:Position, s:Seq[(Int,Int,Animation)], confine:Recti, prob:Double, r:Random) {
  def update:Critter = {
    val newPos = if (r.nextDouble > prob) {
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
    copy(p=newPos, s = s.smap { _.update })
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr `$$>` s.map  { case (i, j, sp) =>
      val f = (t: Tile) => t.setCode(sp.getCode).setFg(sp.getFg)
      (p.x + i, p.y + j, f)
    }
  }
}
