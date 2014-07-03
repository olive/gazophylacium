package in.dogue.gazophylacium.mode.game

import in.dogue.gazophylacium.mode.Mode
import in.dogue.gazophylacium.world.{Room, Player}
import in.dogue.antiqua.graphics.TileRenderer

object GameMode {
  def create(cols:Int, rows:Int) = {
    val p = Player.create(0,0)
    val r = new Room(cols, rows)
    GameMode(cols, rows, p, r)
  }
}

case class GameMode(cols:Int, rows:Int, p:Player, r:Room) extends Mode {
  def update = {
    val pp = p.update
    val ppos = p.move.map{m => r.checkMove(p.p, m)}.getOrElse(p.p)
    copy(p=pp.copy(p=ppos))
  }
  def draw(tr:TileRenderer):TileRenderer = {
    p.draw(tr)
  }
}
