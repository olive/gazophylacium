package in.dogue.gazophylacium.world

import in.dogue.antiqua.ui.MessageBox
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.gazophylacium.input.Controls
import in.dogue.antiqua.Implicits._
import in.dogue.gazophylacium.data._
import scala.Some
import in.dogue.gazophylacium.mode.game.{InTransition, InField, FieldState}

case class Field(m:RoomMap, r:Room, p:Player, t:Option[MessageBox]) {

  def inventory = p.items
  def coords = r.index

  private def updateCurrent:Field = {
    val pp = p.update
    val newT = updateMessage(t)
    val newP = movePlayer(pp)
    val (player, room, map) = updateCollect(newP, r.update)
    copy(p=player, t = newT, r=room, m=map)
  }

  private def movePlayer(p:Player):Player = {
    if (t.isEmpty) {
      val ppos = p.move.map{m => r.checkMove(p.p, m)}.getOrElse(p.p)
      val unstuck = r.checkStuck(ppos)
      p.copy(p=unstuck)
    } else {
      p
    }
  }

  private def updateMessage(t:Option[MessageBox]) = {
    val newT = t.flatMap{_.update}
    val paperOut = Controls.Paper.isPressed
    if (newT.isEmpty && (Controls.Space.justPressed || paperOut)) {
      r.checkRead(p.p.x, p.p.y, paperOut)
    } else {
      newT
    }
  }

  def updateCollect(p:Player, r:Room):(Player, Room, RoomMap) = {
    val (its, room) = r.checkItem(p.p.x, p.p.y)

    (p.collect(its), room, m.collect(its))
  }

  def moveRoom(d:Direction):(Field,Field) = {
    val (newX, newY) = (r.index.x + d.dx, r.index.y + d.dy)

    val newRoom = m(newX, newY)
    newRoom match {
      case None => ??? //fixme
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
