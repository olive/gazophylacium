package in.dogue.gazophylacium.mode

import in.dogue.antiqua.graphics.TileRenderer

trait Mode {
  def update:Mode
  def draw(r:TileRenderer):TileRenderer
}
