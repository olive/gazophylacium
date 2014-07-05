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
  def createRandom(cols:Int, rows:Int, index:Point2i) = {
    val t = MessageBox.create(20, 10, Vector("This is a test\nText box", "this another"), Controls.Space)

    val rd = Readable(14,14, Tile(Code.☼, Color.Black, Color.White), t)
    val tiles = Array2d.tabulate(cols, rows) { case (i, j) =>
      val code = if (Random.nextDouble() > 0.6) {

        Vector(Code.`'`, Code.`,`, Code.`.`, Code.`"`)(Random.nextInt(4))
      } else {
        Code.` `
      }
      val dim = Random.nextFloat() + 1
      val bg = Color.GrossGreen.dim(Random.nextFloat() + 3)
      val c = if (Random.nextDouble() < 0.5) {
        Color.GrossGreen
      } else {
        Color.DarkGreen
      }
      Tile(code, bg, c.dim(dim))
    }
    val r = new Random(0)
    val numTrees = 50
    val trees = (for (i <- 0 until numTrees) yield {
      (Random.nextInt(32), Random.nextInt(32), new Tree(r))
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
    val c = Critter.createSimple(16, 16)
    Room(cols, rows, index, tiles, Seq(rd), stuff.toVector, Seq(c))
  }
}

case class Room(cols:Int, rows:Int, index:Point2i, bg:Array2d[Tile], rds:Seq[Readable], ts:Seq[(Int, Int, Tree)], cs:Seq[Critter]) {


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

  def update:Room = copy(cs = cs.map{_.update})

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

  def checkMove(p:Position, m:Direction):Position = {
    val newPos = p --> m
    if (solidReadable(newPos) || solidTree(newPos)) {
      p.copy(d=m)
    } else {
      p.performMove(m)
    }

  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr.<++(bg.flatten.map{ case (p, q, t) => (p + i, q + j, t)})
      .<++<(rds.map {_.draw(i, j) _})
      .<++<(cs.map {_.draw _})
  }

  def drawFg(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <++< ts.map {case (p, q, t) => t.draw(p,q) _}
  }
}
