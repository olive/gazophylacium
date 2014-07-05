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

object Room {
  def createRandom(worldCols:Int, worldRows:Int, cols:Int, rows:Int, index:Point2i, r:Random) = {
    val t = MessageBox.create(20, 10, Vector("This is a test\nText box", "this another"), Controls.Space)

    val rd = Readable(14,14, Tile(Code.☼, Color.Black, Color.White), t)
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
      Tile(code, bg, c.dim(dim))
    }
    val numTrees = 50
    val trees = (for (i <- 0 until numTrees) yield {
      (r.nextInt(32), r.nextInt(32), new Tree(r))
    }).toVector

    val stuff = ArrayBuffer[(Int, Int, Tree)]()
    def getRect(t:(Int,Int, Tree)) = {
      t._3.getRect(t._1, t._2)
    }
    def oob(rect:Recti) = {
      rect.x < 0 || rect.right >= cols - 1 || rect.y < 0 || rect.bottom >= rows - 1
    }

    for (i <- 0 until numTrees) {
      var found = false
      for (k <- i + 1 until numTrees) {
        val t0 = trees(i)
        val t1 = trees(k)
        if (getRect(t0).intersects(getRect(t1)) || oob(getRect(t0))) {
          found = true
        }
      }
      if (!found) {
        stuff += trees(i)
      }
    }
    val buff = 6
    val cx = buff + r.nextInt(cols - 2*buff)
    val cy = buff + r.nextInt(rows - 2*buff)
    val c = Critter.createSimple(cx, cy, 20, r)
    val d = Machine.create(10, 10, r)
    Room(worldCols, worldRows, cols, rows, index, tiles, Seq(rd), stuff.toVector, Seq(c), d)
  }
}

case class Room(worldCols:Int, worldRows:Int, cols:Int, rows:Int, index:Point2i, bg:Array2d[Tile], rds:Seq[Readable], ts:Seq[(Int, Int, Tree)], cs:Seq[Critter], d:Machine) {


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

  def update:Room = copy(cs = cs.map{_.update}, d=d.update)

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
    ts.exists {case (i, j, t) =>
      val root = t.rootPos(i, j)
      root.x == p.x && root.y == p.y}
  }

  private def isWorldOob(p:Position) = {
    val i = p.x
    val j = p.y
    (
      (i < 0 && index.x == 0)
      || (i > cols - 1 && index.x == worldCols - 1)
      || (j < 0 && index.y == 0)
      || (j >= rows - 1 && index.y == worldRows - 1)
      )
  }

  def checkMove(p:Position, m:Direction):Position = {
    val newPos = p --> m
    if (isWorldOob(newPos) || solidReadable(newPos) || solidTree(newPos)) {
      p.copy(d=m)
    } else {
      p.performMove(m)
    }

  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr.<++(bg.flatten.map{ case (p, q, t) => (p + i, q + j, t)})
      .<++<(rds.map {_.draw(i, j) _})
      .<++<(cs.map {_.draw _})
      .<+<(d.draw(i, j))
  }

  def drawFg(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr //<++< ts.map {case (p, q, t) => t.draw(p,q) _}
  }
}
