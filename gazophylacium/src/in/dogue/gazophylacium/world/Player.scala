package in.dogue.gazophylacium.world

import in.dogue.gazophylacium.graphics.{Tile, TileRenderer}
import in.dogue.gazophylacium.data.Code
import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.input.Control
import in.dogue.gazophylacium.input.Controls
import in.dogue.codepage.Implicits._


object Player {
  def create(i:Int, j:Int) = {
    Player(Position(i, j), Tile(Code.☺, Color.Black, Color.White))
  }
}

case class Player(p:Position, tile:Tile) {
  def move:Option[Move] = {
    def f[T <: AnyVal](c: Control[T]) = c.zip(5, 5)
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

    xMove <|> yMove
  }
  def update:Player = {
    this
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+ (p.x, p.y, tile)
  }
}
