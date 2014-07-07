package in.dogue.gazophylacium.world.doodads

import in.dogue.antiqua.graphics.{TextFactory, Tile, TileRenderer}
import in.dogue.antiqua.data.Code
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.data.Recti
import in.dogue.gazophylacium.Engine
import in.dogue.gazophylacium.world.Position

object Tree {
  def getTrunkColor:Color = Color.Brown
  def getLeafColor:Color = Color.Green
  val stumps = Vector(Code.╜, Code.╨, Code.╙)
  val trunks = Vector(Code.╢, Code.╟, Code.╫, Code.║)
}

case class Tree(trunkBase:Color, leafBase:Color)(r:Random) {
  import Tree._

  val f = TextFactory(Color.Black, Color.White)
  private sealed abstract class BranchType(val i:Int) {
    val tiles:Vector[Tile]
    def depthToLength(d:Int) = d
  }
  private case class Left(trunk:Color, leaf:Color)(depth:Int) extends BranchType(0) {
    val start = Vector(Tile(Code./, Color.Black, leaf))
    val end = Vector(Tile(Code.╢, Color.Black, trunk))
    override val tiles = start ++ end
  }
  private case class Right(trunk:Color, leaf:Color)(depth:Int) extends BranchType(1) {
    val start = Vector(Tile(Code.╟, Color.Black, trunk))
    val end = Vector(Tile(Code.\, Color.Black, leaf))
    override val tiles = start ++ end
  }
  private case class Both(trunk:Color, leaf:Color)(depth:Int) extends BranchType(0) {
    val left = Vector(Tile(Code./, Color.Black, leaf))
    val mid = Vector(Tile(Code.╫, Color.Black, trunk))
    val right = Vector(Tile(Code.\, Color.Black, leaf))
    override val tiles = left ++ mid ++ right
  }
  private case class Neither(trunk:Color)(depth:Int) extends BranchType(1) {
    override val tiles = Vector(Tile(Code.║, Color.Black, trunk))
  }

  private val Branches = IndexedSeq((30, Left(trunkBase, leafBase) _),
                                    (30, Right(trunkBase, leafBase) _),
                                    (30, Both(trunkBase, leafBase) _),
                                    (10, Neither(trunkBase) _)).expand

  private sealed abstract class Segment(iOffset:Int, solid:Boolean) {
    val tiles:Vector[Tile]
    def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
      tr `$$>` tiles.zipWithIndex.map{case (t, k) =>
        val f = { (tile:Tile) => tile.setCode(t.code).setFg(t.fgColor)}
        (i + iOffset + k, j, f)}
    }
  }
  private case class Base(i:Int, trunk:Color) extends Segment(i, true) {
    override val tiles = Vector(Tile(stumps.randomR(r), Color.Black, trunk))
  }
  private case class Mid(t:BranchType, trunk:Color, leaf:Color) extends Segment(t.i, false) {
    override val tiles = t.tiles
  }
  private case class Top(i:Int, leaf:Color) extends Segment(i, false) {
    override val tiles = f.withFg(leaf).create("/│\\").tiles
  }

  private val height = 3 + r.nextInt(5)
  private val base = Base(1, trunkBase)
  private val top = Top(0, leafBase)
  private val mids = (1 until height -1) map { x =>
    val t = Branches.randomR(r)((height - x)/4 + 1)
    Mid(t, trunkBase, leafBase)
  }

  private val segments =  Vector(top) ++ mids.toVector.reverse ++ Vector(base)

  def getRect(i:Int, j:Int):Recti = Recti(i, j, 3, height)

  def rootPos(i:Int, j:Int):Position = {
    Position.create(i + 1, j + height - 1)
  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    //val span = tr.project(getRect(i, j))
    //Engine.r.drawRect(span.x, span.y, span.width, span.height, Color.White)
    tr <++< segments.zipWithIndex.map{ case (s, k) => s.draw(i, j+k) _}
  }

  private def isSolid(i:Int, j:Int)(p:Int, q:Int) = {
    val pos = rootPos(i, j)
    pos.x == p && pos.y == q
  }

  def toDoodad(i:Int, j:Int) = {
    Doodad[Tree](i, j, _.draw, _.getRect, _.isSolid, id[Tree], this)
  }

}
