package in.dogue.gazophylacium.world.doodads

import in.dogue.antiqua.data.Code
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import scala.util.Random
import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.data.Recti
import in.dogue.antiqua.Implicits
import Implicits._

case class Stump(color:Color)(r:Random) {
  val sprite = {
    val code = Tree.stumps.randomR(r)
    Tile(code, Color.Black, color)
  }

  def getRect(i:Int, j:Int) = Recti(i, j, 1, 1)
  def isSolid(i:Int, j:Int)(p:Int, q:Int) = {
    p == i && q == j
  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <| (i, j, sprite)
  }

  def toDoodad(i:Int, j:Int):Doodad[Stump] = {
    Doodad(i, j, _.draw _, _.getRect, _.isSolid _, id[Stump], this)
  }
}
