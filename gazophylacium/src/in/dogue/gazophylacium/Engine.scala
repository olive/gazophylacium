package in.dogue.gazophylacium

import in.dogue.gazophylacium.graphics._
import com.deweyvm.gleany.{GleanyGame, AssetLoader}
import in.dogue.gazophylacium.graphics.Tileset
import in.dogue.gazophylacium.mode.{TitleMode, Mode}
import in.dogue.gazophylacium.input.Controls
import in.dogue.gazophylacium.mode.game.GameMode

class Engine {
  val cols = 32
  val rows = 32
  val ts = Tileset(16, 16, 16, 16, AssetLoader.loadTexture("Md_curses_16x16"))
  val r:Renderer = new Renderer(ts)
  var tr:TileRenderer = TileRenderer.create
  var mode:Mode = GameMode.create(cols, rows)
  def update() {
    if (Controls.Escape.justPressed) {
      GleanyGame.exit()
    }
    mode = mode.update
  }
  def draw() {
    tr = tr <+< mode.draw
    tr = r.render(tr) <-- ()
  }
}
