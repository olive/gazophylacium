package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{TextFactory, Tile, TileRenderer}
import in.dogue.antiqua.data.Code
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.data.Recti

class Tree(r:Random) {
  def getTrunkColor:Color = Color.Brown
  def getLeafColor:Color = Color.Green
  private val trunkBase = getTrunkColor
  private val leafBase = getLeafColor
  val trunks = Vector(Code.╜, Code.╨, Code.╙)
  val f = TextFactory(Color.Black, Color.White)
  private sealed abstract class BranchType(val i:Int) {
    val tiles:Vector[Tile]
    def depthToLength(d:Int) = d
    def makeBranch(c:Code, d:Int, trunk:Color) = (0 until depthToLength(d)).map{_ => Tile(c, Color.Black, trunk)}.toVector
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

  private val Branches = expand(IndexedSeq((30, Left(trunkBase, leafBase) _),
                                           (30, Right(trunkBase, leafBase) _),
                                           (30, Both(trunkBase, leafBase) _),
                                           (10, Neither(trunkBase) _)))


  private def expand[T](s:IndexedSeq[(Int, T)]) = {
    (for ((k, t) <- s) yield {
      for (_ <- 0 until k) yield t
    }).flatten
  }
  private sealed abstract class Segment(iOffset:Int, solid:Boolean) {
    val tiles:Vector[Tile]
    def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
      tr `$$>` tiles.zipWithIndex.map{case (t, k) =>
        val f = { (tile:Tile) => tile.setCode(t.code).setFg(t.fgColor)}
        (i + iOffset + k, j, f)}
    }
  }
  private case class Base(i:Int, trunk:Color) extends Segment(i, true) {
    override val tiles = Vector(Tile(trunks.random(), Color.Black, trunk))
  }
  private case class Mid(t:BranchType, trunk:Color, leaf:Color) extends Segment(t.i, false) {
    override val tiles = t.tiles
  }
  private case class Top(i:Int, leaf:Color) extends Segment(i, false) {
    override val tiles = f.withFg(leaf).create("/|\\").tiles
  }

  private val height = 3 + r.nextInt(5)
  private val base = Base(1, trunkBase)
  private val top = Top(0, leafBase)
  private val mids = (1 until height -1) map { x =>
    val t = Branches.random()((height - x)/4 + 1)
    Mid(t, trunkBase, leafBase)
  }

  private val segments =  Vector(top) ++ mids.toVector.reverse ++ Vector(base)

  def getRect(i:Int, j:Int):Recti = Recti(i, j, 3, height)

  def rootPos(i:Int, j:Int):Position = {
    Position.create(i + 1, j + height - 2)
  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <++< segments.zipWithIndex.map{ case (s, k) => s.draw(i, j+k) _}
  }
}
