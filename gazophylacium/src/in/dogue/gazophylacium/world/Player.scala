package in.dogue.gazophylacium.world

import in.dogue.gazophylacium.input.Controls
import in.dogue.gazophylacium.graphics.{Tile, TileRenderer}
import in.dogue.gazophylacium.data.Code
import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.input.Control
import in.dogue.codepage.Implicits._

object Player {
  def create(i:Int, j:Int) = {
    Player(i, j, Tile(Code.☺, Color.Black, Color.White))
  }
}

case class Player(i:Int, j:Int, tile:Tile) {

  def update:(Player, Option[Move]) = {
    def f[T <: AnyVal](c:Control[T]) = c.zip(5,5)
    val xMove = f(Controls.AxisX) match {
      case 1 => Right.some
      case -1 => Left.some
      case _ => None

    }

    val yMove = f(Controls.AxisY) match {
      case 1 => Down.some
      case -1 => Up.some
      case _ => None

    }

    (this, xMove <+> yMove)
  }

  def <+>[T](x:Option[T], y:Option[T]):Option[T] = x match {
    case xx@Some(_) => xx
    case None => y
  }

  def performMove(move:Move) = copy(i=i+move.dx, j=j+move.dy)

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (i, j, tile)
  }
}
