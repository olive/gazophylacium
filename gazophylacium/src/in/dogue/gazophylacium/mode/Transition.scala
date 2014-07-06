package in.dogue.gazophylacium.mode

import in.dogue.antiqua.graphics.TileRenderer

object Transition {
  def create(cols:Int, rows:Int, m1:Mode, m2:Mode, speed:Int) = {
    Transition(cols, rows, m1, m2, speed, 0)
  }
}

case class Transition(cols:Int, rows:Int, m1:Mode, m2:Mode, speed:Int, t:Int) extends Mode {
  def update:Mode = {
    if (t/speed > rows + cols) {
      m2
    } else {
      copy(t = t+1, m1=m1.update, m2=m2.update)
    }
  }

  def draw(tr:TileRenderer):TileRenderer = {
    val tr1 = tr <+< m1.draw
    val tr2 = tr <+< m2.draw
    val draws = (for (i <- 0 until cols; j <- 0 until rows) yield {
      val r = if (j < i + rows - t/speed) {
        tr1
      } else {
        tr2
      }
      r.draws.get((i, j)).map {t =>
        (i, j, t)
      }
    }).flatten

    tr <++ draws
  }

}
