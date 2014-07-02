package in.dogue.gazophylacium.mode

import in.dogue.gazophylacium.graphics.TileRenderer

trait Mode {
  def update:Mode
  def draw(r:TileRenderer):TileRenderer
}
