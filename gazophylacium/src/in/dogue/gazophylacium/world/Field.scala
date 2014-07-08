package in.dogue.gazophylacium.world

import in.dogue.antiqua.ui.MessageBox
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.gazophylacium.input.Controls
import in.dogue.antiqua.Implicits._
import in.dogue.gazophylacium.data._
import scala.Some
import in.dogue.gazophylacium.mode.game.{InTransition, InField, FieldState}
import in.dogue.gazophylacium.mode.{Mode, EndMode}
import com.deweyvm.gleany.data.Point2d
import in.dogue.gazophylacium.audio.SoundManager
import scala.util.Random

object Field {
  def create(m:RoomMap, r:Room, p:Player) = {
    Field(m, r, p, None, 0)
  }
}

case class Field(m:RoomMap, r:Room, p:Player, mb:Option[MessageBox], t:Int) {

  def inventory = p.items
  def coords = r.index

  def checkMachine(state:Mode):Mode = {
    val machinePos = r.getMachinePos

    machinePos match {
      case Some(rect) if p.paperOut && rect.expand(1, 1).contains(Point2d(p.p.x, p.p.y)) =>
        if (p.hasAllItems) {
          val center = rect.center
          EndMode.create(state, center.toTuple)
        } else {
          if (!SoundManager.wrong.isPlaying) {
            SoundManager.wrong.play()
          }
          state
        }

      case _ => state
    }
  }

  private def updateCurrent:Field = {
    val pp = p.update
    val newT = updateMessage(mb)
    val newP = movePlayer(pp)
    val (player, room, map) = updateCollect(newP, r.update)
    if (t % 240 == 0) {
      SoundManager.trees.randomR(new Random()).play()
    }
    copy(p=player, mb = newT, r=room, m=map, t=t+1)
  }

  private def movePlayer(p:Player):Player = {
    if (mb.isEmpty) {
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
      .<+<(p.drawFg)
      .<+?<(mb.map{_.draw(0,0)}) //fixme
  }
}
