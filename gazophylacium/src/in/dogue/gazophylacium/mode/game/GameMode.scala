package in.dogue.gazophylacium.mode.game

import in.dogue.gazophylacium.mode.Mode
import in.dogue.gazophylacium.world._
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.gazophylacium.ui.Hud
import com.deweyvm.gleany.data.Point2i
import in.dogue.gazophylacium.world.Field
import scala.util.Random

object GameMode {
  def create(cols:Int, rows:Int) = {
    val r = new Random(0)
    val p = Player.create(7,7)
    val roomMap = RoomMap.load(cols, rows, r)
    val start = Point2i(2,2)
    val rm = roomMap(start.x, start.y).get
    val hud = Hud.create(cols, 4, r)
    val field = Field.create(roomMap, rm, p)
    GameMode(cols, rows, InField(field), hud)
  }
}


case class GameMode(cols:Int, rows:Int, state:FieldState, hud:Hud) extends Mode {
  def update = {
    val coords = state.coords
    val inventory = state.inventory
    val newHud = hud.withCoords(coords.x, coords.y).withInventory(inventory)
    val newGm = copy(state=state.update, hud=newHud)
    state.getNewMode(newGm)
  }
  def draw(tr:TileRenderer):TileRenderer = {
    (tr.move(0, hud.height) <+< state.draw).move(0, -hud.height) <+< hud.draw
  }
}
