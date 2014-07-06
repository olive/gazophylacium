package in.dogue.gazophylacium.mode.game

import in.dogue.gazophylacium.mode.Mode
import in.dogue.gazophylacium.world._
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.gazophylacium.ui.Hud
import com.deweyvm.gleany.data.Point2i
import in.dogue.gazophylacium.world.Field

object GameMode {
  def create(cols:Int, rows:Int) = {
    val p = Player.create(7,7)
    val roomMap = RoomMap.load(cols, rows)
    val start = Point2i(0,0)
    val r = roomMap(start.x, start.y).get
    val hud = Hud.create
    val field = Field(roomMap, r, p, None)
    GameMode(cols, rows, InField(field), hud)
  }
}


case class GameMode(cols:Int, rows:Int, state:FieldState, hud:Hud) extends Mode {
  def update = {
    val coords = state.coords
    copy(state=state.update, hud=hud.withCoords(coords.x, coords.y))
  }
  def draw(tr:TileRenderer):TileRenderer = {
    (tr.move(0, 4) <+< state.draw).move(0, -4) <+< hud.draw(0,0)
  }
}
