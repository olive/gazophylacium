package in.dogue.gazophylacium.world

import in.dogue.gazophylacium.data.{Up, Down, Left, Right, Direction}
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.gazophylacium.mode.game.{InField, InTransition, FieldState}

object FieldTransition {
  def create(cols:Int, rows:Int, f0:Field, f1:Field, d:Direction) = {
    FieldTransition(cols, rows, f0, f1, d, 0)
  }
}

case class FieldTransition(cols:Int, rows:Int, f0:Field, f1:Field, d:Direction, t:Int) {
  val speed = 1
  val limit = if (d == Up || d == Down) {
    rows
  } else {
    cols
  }
  def coords = f0.r.index

  def update:FieldState = {
    val newT = t+1
    if (newT/speed > limit) {
      InField(f1)
    } else {
      InTransition(copy(t=newT))
    }
  }

  def getMoves(d:Direction):((Int,Int),(Int,Int)) ={
    d match {
      case Right =>
        ((-t/speed, 0), (cols - t/speed, 0))
      case Down =>
        ((0, -t/speed), (0, rows - t/speed))
      case Left =>
        ((t/speed, 0), (t/speed - cols, 0))
      case Up =>
        ((0, t/speed), (0, t/speed - rows))
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    val ntr = tr ^^^ ()
    val offset = (ntr.originX, ntr.originY)
    val (m0, m1) = getMoves(d)
    val d0 = ntr.att(m0).movet(offset) <+< f0.draw
    val d1 = ntr.att(m1).movet(offset) <+< f1.draw

    tr <*< d0 <*< d1
  }
}
