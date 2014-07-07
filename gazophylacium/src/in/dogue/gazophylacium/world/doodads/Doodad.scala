package in.dogue.gazophylacium.world.doodads

import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.data.Recti

case class Doodad[T](i:Int,
                     j:Int,
                     f:T => (Int,Int) => TileRenderer => TileRenderer,
                     rect:T => (Int,Int) => Recti,
                     solid:T => (Int,Int) => (Int,Int) => Boolean,
                     up:T => T,
                     t:T) {
  /**
   * Is the given doodad solid given that the doodad's position is i,j
   * and the point to be tested is p,q
   */
  def isSolid(p:Int, q:Int) = solid(t)(i, j)(p, q)
  def update:Doodad[T] = copy(t=up(t))
  def getRect:Recti = rect(t)(i, j)
  def draw(tr:TileRenderer):TileRenderer = tr <+< f(t)(i, j)
}
