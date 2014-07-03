package in.dogue.gazophylacium.mode.game

import in.dogue.gazophylacium.graphics.TileRenderer
import in.dogue.gazophylacium.mode.Mode
import in.dogue.gazophylacium.world.Player

object GameMode {
  def create(cols:Int, rows:Int) = {
    val p = Player.create(0,0)
    GameMode(cols, rows, p)
  }
}

case class GameMode(cols:Int, rows:Int, p:Player) extends Mode {

  def update = {
    val (pp, m) = p.update
    copy(p=m.map{d => pp.performMove(d)}.getOrElse(pp))
  }
  def draw(tr:TileRenderer):TileRenderer = {
    p.draw(tr)
  }
}
