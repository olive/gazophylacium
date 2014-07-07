package in.dogue.gazophylacium.world

import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.input.Control
import in.dogue.gazophylacium.input.Controls
import in.dogue.antiqua.Implicits._
import in.dogue.antiqua.data.Code
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import in.dogue.gazophylacium.data._
import in.dogue.antiqua.graphics.Tile


object Player {
  def create(i:Int, j:Int) = {
    Player(Position.create(i, j), Tile(Code.☺, Color.Black, Color.White), Seq())
  }
}

case class Player(p:Position, tile:Tile, items:Seq[Item]) {

  def collect(s:Seq[Item]) = {
    copy(items = items ++ s)
  }
  def setPos(pos:Position) = copy(p=pos)

  def move:Option[Direction] = {
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

  def paper = Tile(Code.■, Color.Black, Color.Green)
  def tail = Tile(Code.σ, Color.Black, Color.White)
  def draw(tr:TileRenderer):TileRenderer = {
    val pDraw = if (Controls.Paper.isPressed) {
      val ppos = p.performMove(p.d)
      (ppos.x, ppos.y, paper).some
    } else {
      None
    }
    tr <+ (p.x, p.y, tile) <+ (p.prevX, p.prevY, tail) <+? pDraw
  }
}
