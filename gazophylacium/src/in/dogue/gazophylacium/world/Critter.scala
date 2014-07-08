package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{Rect, Tile, TileRenderer, Animation}
import in.dogue.antiqua.data.Code
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.gazophylacium.data.Direction
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.data.{Point2d, Recti}
import in.dogue.gazophylacium.audio.SoundManager

object Critter {
  private def create(i:Int, j:Int, anim:Seq[(Int,Int,Animation)], sound:() => Unit, roamSize:Int, prob:Double, r:Random) = {
    val rs = roamSize/2
    val rect = Recti(i - rs, j - rs, roamSize, roamSize)
    Critter(Position.create(i, j), anim, sound, rect, prob, r)

  }

  private def simpleAnimal(fg:Color, c1:Code, c2:Code, speed:Int)(i:Int, j:Int, roamSize:Int, prob:Double, r:Random) = {
    val t1 = Tile(c1, Color.Black/*unused*/, fg)
    val t2 = Tile(c2, Color.Black/*unused*/, fg)
    val anim = Animation.makeBlinker(speed, Vector(t1, t2))
    create(i, j, Seq((0,0,anim)), SoundManager.stepSmall.play, roamSize, prob, r)
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
    def mkTile(c:Code) = Tile(c, Color.Black, Color.Green)
    val headAnim = Animation.create(Vector(
      (30, mkTile(Code.Ö_u)),
      (3, mkTile(Code.ö))
    ))
    val body = Seq(
      (0, 0, Animation.singleton(mkTile(Code.`≤`))),
      (1, 0, headAnim),
      (2, 0, Animation.singleton(mkTile(Code.`≥`)))

    )
    Critter.create(i, j, body, SoundManager.stepMed.play, roamSize, 0.99, r)
  }

  def createDragonfly(i:Int, j:Int, roamSize:Int, r:Random) = {
    val bg = Color.Black
    val fg = Color.Red
    def mkTile(c:Code) = Tile(c, bg, fg)
    val rwing = Animation.create(Vector(
      (3, mkTile(Code.<)),
      (3, mkTile(Code.`«`))

    ))
    val lwing = Animation.create(Vector(
      (3, mkTile(Code.>)),
      (3, mkTile(Code.`»`))
    ))
    val head = Animation.singleton(mkTile(Code.`¡`))
    val body = Seq(
      (0, 0, lwing),
      (1, 0, head),
      (2, 0, rwing)

    )
    Critter.create(i, j, body, SoundManager.stepSmall.play, roamSize, 0.80, r)
  }

  def createBird(i:Int, j:Int, roamSize:Int, r:Random) = {
    simpleAnimal(Color.Cyan, Code.^, Code.-, 15)(i, j, roamSize, 0.9, r)
  }

  def createSnake(i:Int, j:Int, roamSize:Int, r:Random) = {
    simpleAnimal(Color.Green, Code.~, Code.-, 15)(i, j, roamSize, 0.99, r)
  }

  def createBat(i:Int, j:Int, roamSize:Int, r:Random) = {
    val bg = Color.Black
    val fg = Color.Tan
    def mkTile(c:Code) = Tile(c, bg, fg)
    val lwing = Animation.create(Vector(
      (30, mkTile(Code.^)),
      (30, mkTile(Code.⌐))
    ))
    val rwing = Animation.create(Vector(
      (30, mkTile(Code.^)),
      (30, mkTile(Code.¬))
    ))
    val head = Animation.singleton(mkTile(Code.°))
    val body = Seq(
      (0, 0, lwing),
      (1, 0, head),
      (2, 0, rwing)

    )
    Critter.create(i, j, body, SoundManager.stepMed.play, roamSize, 0.99, r)
  }

  def createLizard(i:Int, j:Int, roamSize:Int, r:Random) = {
    val bg = Color.Black
    val fg = Color.LightGreen
    def mkTile(c:Code) = Tile(c, bg, fg)
    val leg0_0 = mkTile(Code.`[`)
    val leg0_1 = mkTile(Code.│)
    val leg0_2 = mkTile(Code.`]`)
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
    val trunk = Animation.singleton(mkTile(Code.`=`))
    val head = Animation.singleton(mkTile(Code.`º`))
    val body = Seq(
      (0, 0, legAnim0),
      (1, 0, trunk),
      (2, 0, legAnim1),
      (3, 0, head)

    )
    Critter.create(i, j, body, SoundManager.stepBig.play, roamSize, 0.99, r)
  }

  def createDino(i:Int, j:Int, roamSize:Int, r:Random) = {
    val bg = Color.Black
    val fg = Color.Grey
    def mkTile(c:Code) = Tile(c, bg, fg)

    val tail = Animation.create(Vector(
      (17, mkTile(Code.~)),
      (13, mkTile(Code.-))
    ))
    val body = Animation.singleton(mkTile(Code.`=`))
    val lfoot = Animation.create(Vector(
      (19, mkTile(Code.-)),
      (19, mkTile(Code.^))
    ))
    val rfoot = Animation.create(Vector(
      (19, mkTile(Code.^)),
      (19, mkTile(Code.-))
    ))

    val eye = Animation.singleton(mkTile(Code.°))
    val mouth = Animation.create(Vector(
      (3, mkTile(Code.-)),
      (61, mkTile(Code.<))
    ))

    val all = Seq(
      (0, 0, tail),
      (1, 0, body),
      (2, 0, body),
      (3, 0, eye),
      (4, 0, mouth),
      (1, 1, lfoot),
      (2, 1, rfoot)

    )
    Critter.create(i, j, all, SoundManager.stepBig.play, roamSize, 0.99, r)
  }


}

case class Critter(p:Position, s:Seq[(Int,Int,Animation)], sound:() => Unit, confine:Recti, prob:Double, r:Random) {
  def update:Critter = {
    val newPos = if (r.nextDouble > prob) {
      val d = Direction.All.randomR(r)
      val newPos = p --> d
      if (confine.contains(Point2d(newPos.x, newPos.y))) {
        sound()
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
