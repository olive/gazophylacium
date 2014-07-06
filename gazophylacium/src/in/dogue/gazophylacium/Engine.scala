package in.dogue.gazophylacium

import com.deweyvm.gleany.{GleanyGame, AssetLoader}
import in.dogue.gazophylacium.mode.Mode
import in.dogue.gazophylacium.mode.game.GameMode
import in.dogue.antiqua.graphics.{Renderer, Tileset, TileRenderer}
import in.dogue.gazophylacium.input.Controls

object Engine {
  val ts = Tileset(16, 16, 16, 16, AssetLoader.loadTexture("Md_curses_16x16"))
  val r = new Renderer(ts)
}

class Engine {
  import Engine._
  val roomCols = 32
  val roomRows = 31
  var tr:TileRenderer = TileRenderer.create
  var mode:Mode = GameMode.create(roomCols, roomRows)
  def update() {
    if (Controls.Escape.justPressed) {
      GleanyGame.exit()
    }
    mode = mode.update
  }
  def draw() {
    tr = tr <+< mode.draw
    tr = r.render(tr) ^^^ ()
  }
}
