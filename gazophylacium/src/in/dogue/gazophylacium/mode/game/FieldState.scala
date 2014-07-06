package in.dogue.gazophylacium.mode.game

import in.dogue.gazophylacium.world.{Field, FieldTransition}
import in.dogue.antiqua.graphics.TileRenderer
import com.deweyvm.gleany.data.Point2i

sealed trait FieldState {
  def coords:Point2i
  def update:FieldState
  def draw(tr:TileRenderer):TileRenderer
}
case class InTransition(f:FieldTransition) extends FieldState {
  override def coords = f.coords
  override def update = f.update
  override def draw(tr:TileRenderer) = tr <+< f.draw
}
case class InField(f:Field) extends FieldState {
  override def coords = f.coords
  override def update = f.update
  override def draw(tr:TileRenderer) = tr <+< f.draw
}
