package in.dogue.gazophylacium.mode

import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.data.Code
import in.dogue.antiqua.graphics.{TileRenderer, Tile}
import scala.util.Random
import in.dogue.antiqua.Implicits._

object Interim {
  def create(cols:Int, rows:Int, dest:Mode, speed:Int) = {
    Interim(cols, rows, dest, speed, 0, new Random())
  }
}
case class Interim(cols:Int, rows:Int, dest:Mode, speed:Int, t:Int, r:Random) extends Mode {

  def update = {
    if (t/speed > rows*2) {
      Transition(cols, rows, this, dest, speed, 0)
    } else {
      copy(t=t+1)
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr <++ (for (i <- 0 until cols; j <- 0 until rows) yield {
      (i, j, Tile(Code.All.randomR(r), Color.White, Color.Black))
    })
  }
}
