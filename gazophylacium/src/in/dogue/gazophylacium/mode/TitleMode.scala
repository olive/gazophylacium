package in.dogue.gazophylacium.mode

import in.dogue.gazophylacium.input.Controls
import in.dogue.gazophylacium.mode.game.GameMode
import in.dogue.antiqua.graphics.{TileRenderer, TextFactory, Border}
import com.deweyvm.gleany.graphics.Color

class TitleMode(cols:Int, rows:Int) extends Mode {
  val tf = TextFactory.bw
  val b = Border.standard(Color.Black, Color.White)(cols, rows)
  val t = tf.create("»GAZOPHYLACIUM")
  def update = {
    if (Controls.Space.justPressed) {
      val speed = 1
      val newMode = GameMode.create(cols,rows)
      Transition.create(cols, rows, this, Interim.create(cols, rows, newMode, speed), speed)
    } else {
      this
    }

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< b.draw(0,0) <+< t.draw((cols - t.length)/2,rows/2 - 1)
  }

}
