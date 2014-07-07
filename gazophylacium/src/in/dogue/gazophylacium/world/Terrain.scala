package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{Animation, TileRenderer}
import in.dogue.antiqua.data.Array2d
import in.dogue.antiqua.Implicits
import Implicits._

case class Terrain(tiles:Seq[(Int,Int,Animation)], solid:Array2d[Boolean]) {
  def update:Terrain = copy(tiles=tiles.smap{_.update})
  def isSolid(i:Int, j:Int) = {
    solid.getOption(i, j).getOrElse(false)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <## tiles
  }
}
