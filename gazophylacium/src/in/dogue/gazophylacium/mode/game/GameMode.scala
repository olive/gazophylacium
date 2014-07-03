package in.dogue.gazophylacium.mode.game

import in.dogue.gazophylacium.mode.Mode
import in.dogue.gazophylacium.world.{Room, Player}
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.antiqua.ui.{MessageBox, TextBox}

object GameMode {
  def create(cols:Int, rows:Int) = {
    val p = Player.create(0,0)
    val r = new Room(cols, rows)
    val t = MessageBox.create(20,10, Vector("This is a test\nText box"))
    GameMode(cols, rows, p, r, t)
  }
}

case class GameMode(cols:Int, rows:Int, p:Player, r:Room, t:MessageBox) extends Mode {
  def update = {
    val pp = p.update
    val ppos = p.move.map{m => r.checkMove(p.p, m)}.getOrElse(p.p)
    copy(p=pp.copy(p=ppos))
  }
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< p.draw <+< t.draw(0,0)
  }
}
