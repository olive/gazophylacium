package in.dogue.gazophylacium.mode

import in.dogue.gazophylacium.input.Controls
import in.dogue.gazophylacium.mode.game.GameMode
import in.dogue.antiqua.graphics._
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import in.dogue.antiqua.data.Code
import in.dogue.antiqua.Implicits
import Implicits._

case class TitleMode(screenCols:Int, screenRows:Int, roomCols:Int, roomRows:Int) extends Mode {
  val tf = TextFactory.bw
  val b = Border.standard(Color.Black, Color.White)(screenCols, screenRows)
  val t = tf.create("»GAZOPHYLACIUM")
  val rand = new Random()
  val r:Rect = {
    def makeTile(r:Random) = {
      val code = Vector(Code.`.`, Code.`0`, Code.o, Code.O, Code.Θ, Code.°, Code.●).randomR(r)
      val bg =  Color.Orange.dim(7 + r.nextDouble)
      val fg = Color.Yellow.dim(5 + r.nextDouble())
      Tile(code, bg, fg)
    }
    Rect.createTextured(screenCols, screenRows, makeTile, rand)
  }
  def update = {
    if (Controls.Space.justPressed) {
      val speed = 1
      val newMode = GameMode.create(roomCols,roomRows)
      Transition.create(screenCols, screenRows, this, Interim.create(screenCols, screenRows, newMode, speed), speed)
    } else {
      this
    }

  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <+< r.draw(0,0) <+< b.draw(0,0) <+< t.drawFg((screenCols - t.length)/2,screenRows/2 - 1)
  }

}
