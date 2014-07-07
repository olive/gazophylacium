package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{Animation, Tile, TileRenderer}
import in.dogue.antiqua.ui.MessageBox
import in.dogue.antiqua.Implicits._
import in.dogue.antiqua.data.{Code, Array2d}
import in.dogue.gazophylacium.input.Controls
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import com.deweyvm.gleany.data.{Recti, Point2i}
import in.dogue.gazophylacium.data._
import scala.Some
import in.dogue.gazophylacium.world.doodads.{Tree, Doodad, Machine}

object Room {
  def createRandom(worldCols:Int, worldRows:Int, cols:Int, rows:Int, index:Point2i, r:Random) = {
    val t = MessageBox.create(20, 10, Vector("This is a test\nText box", "this another"), Controls.Space)

    val rd = Readable(14,14, Tile(Code.☼, Color.Black, Color.Brown), t)
    val tiles = Array2d.tabulate(cols, rows) { case (i, j) =>
      val code = if (r.nextDouble > 0.6) {

        Vector(Code.`'`, Code.`,`, Code.`.`, Code.`"`)(r.nextInt(4))
      } else {
        Code.` `
      }
      val dim = r.nextFloat() + 1
      val bg = Color.GrossGreen.dim(r.nextFloat() + 3)
      val c = if (r.nextDouble() < 0.5) {
        Color.GrossGreen
      } else {
        Color.DarkGreen
      }
      Animation.singleton(Tile(code, bg, c.dim(dim)))
    }

    val wcols = 20
    val wrows = 20
    val water = WaterBlob.create(wcols, wrows, 12, wcols*2, r)
    val wx = 10
    val wy = 10
    val ttiles = tiles.map { case (i, j, tile) =>
      water.mask.getOption(i - wx, j - wy).flatten match {
        case Some(w) => w
        case None => tile
      }
    }

    val solid = tiles.map { case (i, j, tile) =>
      water.mask.getOption(i - wx, j - wy).flatten match {
        case Some(w) => true
        case None => false
      }
    }

    val numTrees = 50
    val allTrees = (for (i <- 0 until numTrees) yield {
      (r.nextInt(cols), r.nextInt(rows), new Tree(r))
    }).toVector

    val rawTrees = ArrayBuffer[(Int, Int, Tree)]()
    def getRect(t:(Int,Int, Tree)) = {
      t._3.getRect(t._1, t._2)
    }
    def oob(rect:Recti) = {
      rect.x < 0 || rect.right >= cols - 1 || rect.y < 0 || rect.bottom >= rows - 1
    }

    val waterRect = Recti(wx, wy, 0, 0) + water.span
    def inWater(rect:Recti) = {
      rect.intersects(waterRect)
    }

    val d = Machine.create(10, 10, r).toDoodad(r.nextInt(cols - 10), r.nextInt(rows - 10))
    for (i <- 0 until numTrees) {
      var found = false
      val t0 = allTrees(i)
      val water = inWater(getRect(t0))
      if (getRect(t0).intersects(d.getRect) || water) {
        found = true
      }
      if (!found) {
        for (k <- i + 1 until numTrees) {

          val t1 = allTrees(k)
          val isOob = oob(getRect(t0))
          val intersects = getRect(t0).intersects(getRect(t1))

          if (intersects || isOob) {
            found = true
          }
        }
      }
      if (!found) {
        rawTrees += allTrees(i)
      }
      ()
    }
    val buff = 6
    val cx = buff + r.nextInt(cols - 2*buff)
    val cy = buff + r.nextInt(rows - 2*buff)
    val c = Critter.createRandom(cx, cy, 20, r)
    val trees = rawTrees.map { case (i, j, t) => t.toDoodad(i, j)} ++ Vector(d)
    val item = Item.create(0, 0, Animation.create(Vector((1, Tile(Code.t, Color.Black, Color.White)))))


    val terrain = Terrain(ttiles.flatten, solid)

    Room(worldCols, worldRows, cols, rows, index, terrain, Seq(rd), trees.toVector, Seq(c), Seq(item))
  }
}

case class Room(worldCols:Int, worldRows:Int, cols:Int, rows:Int, index:Point2i, terrain:Terrain, rds:Seq[Readable], ts:Seq[Doodad[_]], cs:Seq[Critter], is:Seq[Item]) {

  def load(info:RoomInfo) = {
    copy(is=is.filter{it => !info.contains(it)})
  }

  def getOob(p:Position):Option[Direction] = {
    val i = p.x
    val j = p.y
    if (i > cols - 1) {
      Right.some
    } else if (i < 0) {
      Left.some
    } else if (j > rows - 1) {
      Down.some
    } else if (j < 0) {
      Up.some
    } else {
      None
    }
  }

  def update:Room = {
    val newCs = cs.map{_.update}
    val newTs = ts.map{_.update}
    val newIs = is.map{_.update}
    val newTerrain = terrain.update
    copy(cs=newCs, ts=newTs, is=newIs, terrain=newTerrain)
  }

  def checkItem(i:Int, j:Int):(Seq[Item],Room) = {
    def isItem(it:Item, i:Int, j:Int) = it.i == i && it.j == j
    val found = is.find{it => isItem(it, i, j)}
    if (found.isDefined) {
      val newItems = is.filter{it => !isItem(it, i, j)}
      (Seq(found).flatten, copy(is=newItems))
    } else {
      (Seq(), this)
    }
  }

  def checkRead(i:Int, j:Int, paperOut:Boolean):Option[MessageBox] = {
    val boxes = for ((p, q) <- Seq((i + 1, j), (i - 1, j), (i, j - 1), (i, j + 1))) yield {
      rds.find{_.isPos(Position.create(p, q))} match {
        case Some(rd) =>
          if (paperOut) {
            rd.paper.some
          } else {
            rd.standard.some
          }
        case None => None

      }
    }
    boxes.flatten.headOption
  }

  private def solidReadable(p:Position) = {
    rds.exists{_.isPos(p)}
  }

  private def solidTree(p:Position) = {
    ts.exists {t =>
      t.isSolid(p.x, p.y)
    }
  }

  private def isWorldOob(p:Position) = {
    val i = p.x
    val j = p.y
    (
      (i < 0 && index.x == 0)
      || (i > cols - 1 && index.x == worldCols - 1)
      || (j < 0 && index.y == 0)
      || (j > rows - 1 && index.y == worldRows - 1)
      )
  }

  private def isRoomOob(p:Position) = {
    val i = p.x
    val j = p.y
    i < 0 || i > cols - 1 || j < 0 || j > rows - 1
  }

  private def isSolid(p:Position):Boolean = {
    isWorldOob(p) || solidReadable(p) || solidTree(p) || terrain.isSolid(p.x, p.y)
  }

  private def isStuck(p:Position):Boolean = {
    val poses = Vector(p --> Right, p --> Up, p --> Down, p --> Left)
    poses.forall(isSolid) || isSolid(p)
  }

  def checkMove(p:Position, m:Direction):Position = {
    val newPos = p --> m
    if (isSolid(newPos)) {
      p.copy(d=m)
    } else {
      p.performMove(m)
    }
  }

  def checkStuck(p:Position):Position = {
    var pp = p
    while (isStuck(pp)) {
      pp = pp --> p.d
      if (isRoomOob(pp)) {
        for (i <- 0 until cols; j <- 0 until rows) {
          val pos = Position.create(i, j)
          if (!isStuck(pos)) {
            return pos
          }
        }
      }
    }
    pp
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr.<+<(terrain.draw)
      .<++<(rds.map {_.draw(0,0) _}) //fixme
      .<++<(cs.map {_.draw _})
  }

  def drawFg(tr:TileRenderer):TileRenderer = {
    tr <++< ts.map {_.draw _} <++< is.map {_.draw _}
  }
}
