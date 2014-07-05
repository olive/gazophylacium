package in.dogue.gazophylacium.world

import in.dogue.gazophylacium.data.{Up, Direction}

object Position {
  def create(i:Int, j:Int) = {
    Position(i, j+1, i, j, Up)
  }
}

case class Position(prevX:Int, prevY:Int, x:Int, y:Int, d:Direction) {
  def -->(m:Direction) = Position(x, y, m.dx + x, m.dy + y, m)
  def performMove(m:Direction):Position = {
    this --> m
  }

  def setX(nx:Int) = Position(x, y, nx, y, d)
  def setY(ny:Int) = Position(x, y, x, ny, d)
}

