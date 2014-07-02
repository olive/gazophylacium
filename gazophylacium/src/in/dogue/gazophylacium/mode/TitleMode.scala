package in.dogue.gazophylacium.mode

import in.dogue.gazophylacium.graphics.{TextFactory, TileRenderer, Border}
import in.dogue.gazophylacium.input.Controls

class TitleMode(cols:Int, rows:Int) extends Mode {
  val tf = TextFactory.bw
  val b = Border.standard(cols, rows)
  val t = tf.create("»GAZOPHYLACIUM")
  def update = {
    if (Controls.Pattern.justPressed) {
      val newMode = new TitleMode(cols,rows)
      Interim.create(cols, rows, newMode, 1)
    } else {
      this
    }

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< b.draw(0,0) <+< t.draw((cols - t.length)/2,rows/2 - 1)
  }

}
