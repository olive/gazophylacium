package in.dogue.gazophylacium.mode

import in.dogue.codepage.graphics.TileRenderer

trait Mode {
  def update:Mode
  def draw(r:TileRenderer):TileRenderer
}
