package in.dogue.gazophylacium.world

import in.dogue.antiqua.ui.MessageBox
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.gazophylacium.input.Controls
import in.dogue.antiqua.Implicits._
import in.dogue.gazophylacium.data._
import scala.Some
import in.dogue.gazophylacium.mode.game.{InTransition, InField, FieldState}

case class Field(m:RoomMap, r:Room, p:Player, t:Option[MessageBox]) {

  def coords = r.index

  private def updateCurrent:Field = {
    val pp = p.update
    val newT = t.flatMap{_.update}
    val paperOut = Controls.Paper.isPressed
    val newT2 = if (newT.isEmpty && (Controls.Space.justPressed || paperOut)) {
      r.checkRead(p.p.x, p.p.y, paperOut)
    } else {
      newT
    }
    val newP = if (t.isEmpty) {
      val ppos = p.move.map{m => r.checkMove(p.p, m)}.getOrElse(p.p)
      pp.copy(p=ppos)
    } else {
      p
    }
    copy(p=newP, t = newT2, r=r.update)
  }

  def moveRoom(d:Direction):(Field,Field) = {
    val (newX, newY) = (r.index.x + d.dx, r.index.y + d.dy)

    val newRoom = m(newX, newY)
    newRoom match {
      case None => ???
      case Some(room) =>
        val cols = room.cols
        val rows = room.rows
        val newPos = d match {
          case Left => p.p.setX(cols - 1)
          case Right => p.p.setX(0)
          case Up =>p.p.setY(rows - 1)
          case Down => p.p.setY(0)
        }

        (this, copy(r=room, p=p.setPos(newPos)))
    }
  }

  def update:FieldState = {
    r.getOob(p.p) match {
      case Some(d) =>
        val (f0, f1) = moveRoom(d)
        InTransition(FieldTransition.create(r.cols, r.rows, f0, f1, d))
      case None =>
        InField(updateCurrent)
    }

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr.<+<(r.draw)
      .<+<(p.draw)
      .<+<(r.drawFg)
      .<+?<(t.map{_.draw(0,0)}) //fixme
  }
}
