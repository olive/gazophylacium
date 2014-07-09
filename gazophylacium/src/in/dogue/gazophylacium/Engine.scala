package in.dogue.gazophylacium

import com.deweyvm.gleany.{GleanyGame, AssetLoader}
import in.dogue.gazophylacium.mode.{TitleMode, Mode}
import in.dogue.antiqua.graphics.{Renderer, Tileset, TileRenderer}
import in.dogue.gazophylacium.input.Controls

object Engine {
  val ts = Tileset(16, 16, 16, 16, AssetLoader.loadTexture("Md_curses_16x16"))
  val r = new Renderer(ts)
}

class Engine {
  import Engine._
  val roomCols = 32
  val roomRows = 28
  var tr:TileRenderer = TileRenderer.create
  var mode:Mode = TitleMode(Game.ScreenWidth, Game.ScreenHeight, roomCols, roomRows)
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
