package in.dogue.gazophylacium.mode

import in.dogue.gazophylacium.mode.game.GameMode
import in.dogue.gazophylacium.graphics.Effect
import in.dogue.antiqua.graphics.TileRenderer
import in.dogue.gazophylacium.Game

object EndMode {
  def create(gm:Mode, epicenter:(Int,Int)) = {
    EndMode(gm, Effect.create(Game.ScreenWidth, Game.ScreenHeight, epicenter))
  }
}

case class EndMode(gm:Mode, effect:Effect) extends Mode {
  def update = copy(effect=effect.update)
  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< gm.draw <+< effect.draw
  }
}
