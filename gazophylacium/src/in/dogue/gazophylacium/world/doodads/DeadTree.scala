package in.dogue.gazophylacium.world.doodads

import scala.util.Random
import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Implicits._
import in.dogue.antiqua.graphics.Tile
import scala.collection.immutable.IndexedSeq

case class DeadTree(color:Color)(r:Random) {
  val height = 2 + r.nextInt(7)
  val sprites: IndexedSeq[Tile] = {
    val mids = (0 until height - 1).map { i =>
      Tile(Tree.trunks.randomR(r), Color.Black, color)
    }
    val base = Tile(Tree.stumps.randomR(r), Color.Black, color)
    mids :+ base
  }

  def getRect(i: Int, j: Int) = Recti(i, j, 1, height)

  def isSolid(i: Int, j: Int)(p: Int, q: Int) = {
    p == i && q == j + height - 1
  }

  def drawBg(i: Int, j: Int)(tr: TileRenderer): TileRenderer = {
    tr
  }

  def draw(i: Int, j: Int)(tr: TileRenderer): TileRenderer = {
    val ss = sprites.zipWithIndex.map { case (s:Tile, k:Int) => (i, j + k, s)}
    tr <|| ss
  }

  def toDoodad(i: Int, j: Int): Doodad[DeadTree] = {
    Doodad(i, j, _.draw, _.drawBg, _.getRect, _.isSolid _, id[DeadTree], this)
  }
}
