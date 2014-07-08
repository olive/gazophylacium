package in.dogue.antiqua.ui

import in.dogue.antiqua.graphics._
import in.dogue.antiqua.graphics.Text
import com.deweyvm.gleany.graphics.Color
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.input.Control
import in.dogue.antiqua.data.Code
import in.dogue.gazophylacium.input.Controls
import in.dogue.gazophylacium.world.Room.ColorScheme
import scala.util.Random

object Line {
  def create(v:Text) = Line(v, 0)
}
case class Line(v:Text, ptr:Int) {
  def isFinished = ptr >= v.length
  def update = copy(ptr=Math.min(v.length, ptr+1))
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< v.drawFgSub(ptr)(i, j)
  }
}


case class TextBox(lines:Vector[Line], ptr:Int) {

  def atEnd = ptr >= lines.length
  private def updateLast() = {
    lines.updated(ptr, lines(ptr).update)
  }
  def update:TextBox = {
    if (!atEnd && lines(ptr).isFinished) {
      copy(ptr=ptr+1)
    } else if (ptr < lines.length) {
      copy(lines=updateLast())
    } else {
      this
    }
  }
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    val bound = Math.min(lines.length, ptr + 1)
    tr <++< (for (k <- 0 until bound) yield {
      lines(k).draw(i, j+k) _
    })
  }

  def isFinished:Boolean = atEnd && lines(ptr-1).isFinished
}

object MessageBox {

  def createSpace(cols:Int, rows:Int, xs:Vector[String], bcs:BoxColorScheme, rect:Rect, r:Random) = {
    create(cols, rows, xs, rect, bcs, Controls.Space, r)
  }

  def createPlain(cols:Int, rows:Int, xs:Vector[String], bcs:BoxColorScheme, progress:Control[Boolean], r:Random) = {
    val bg = Rect.createPlain(cols, rows, Tile(Code.` `, Color.Blue, Color.Blue))
    create(cols, rows, xs, bg, bcs, progress, r)
  }

  private def create(cols:Int, rows:Int, xs:Vector[String], rect:Rect, bcs:BoxColorScheme, progress:Control[Boolean], r:Random): MessageBox = {
    val textColor = bcs.text.getColor(r)
    val f = TextFactory(Color.Black, textColor)
    val b = Border.standard(bcs.bg.getColor(r), bcs.text.getColor(r))(cols, rows)
    val boxes = xs map {_.split("\n").toVector}
    val all = boxes map {b => TextBox(b.map(bb => Line.create(f.create(bb))), 0)}
    val arrow = f.create("▼")
    MessageBox(all, rect, b, 0, arrow, progress, Intro(0), 0)

  }
}


object BoxColorScheme {
  val standard = {
    val border = ColorScheme(Vector(Color.Grey), 1, 1)
    val bg = ColorScheme(Vector(Color.DarkGrey), 1, 1)
    val text = border
    BoxColorScheme(border, bg, text)
  }

  val page = {
    val border = ColorScheme(Vector(Color.DarkGreen), 3, 1)
    val bg = ColorScheme(Vector(Color.DarkGreen), 3, 1)
    val text = ColorScheme(Vector(Color.Green), 1, 1)
    BoxColorScheme(border, bg, text)
  }
}

case class BoxColorScheme(border:ColorScheme, bg:ColorScheme, text:ColorScheme)


sealed trait BoxState
case class Intro(t:Int) extends BoxState
case object Reading extends BoxState
case class Outro(t:Int) extends BoxState
case object Done extends BoxState

case class MessageBox(bs:Vector[TextBox], bg:Rect, b:Border, ptr:Int, arrow:Text, progress:Control[Boolean], state:BoxState, t:Int) {
  val speed=1
  def isFinished = bs(ptr).isFinished && ptr >= bs.length - 1

  private def last = bs(ptr)

  private def updateLast() = {
    bs.updated(ptr, last.update)
  }

  private def updateIntro(i:Intro):MessageBox = {
    if (i.t/speed >= (b.cols + b.rows) - 1) {
      copy(state=Reading)
    } else {
      copy(state=Intro(i.t + 1))
    }
  }

  private def updateOutro(o:Outro):MessageBox = {
    if (o.t/speed > (b.cols + b.rows)) {
      copy(state=Done)
    } else {
      copy(state=Outro(o.t + 1))
    }
  }

  private def updateBox:MessageBox = {
    val next = if (isFinished && progress.justPressed) {
      copy(state=Outro(0))
    } else {
      if (last.isFinished && progress.justPressed) {
        copy(ptr = ptr+1)
      } else {
        copy(bs=updateLast())
      }
    }
    next.copy(t=t+1)
  }

  def update:Option[MessageBox] = {
    state match {
      case i@Intro(_) => updateIntro(i).some
      case Reading => updateBox.some
      case o@Outro(_) => updateOutro(o).some
      case Done => None
    }
  }

  private def drawArrow(tr:TileRenderer):TileRenderer = {
    if (last.isFinished && ptr < bs.length - 1 && t % 30 < 15) {
      tr <+< arrow.drawFgSub(1)(b.cols-2,b.rows-2)
    } else {
      tr
    }
  }

  def drawBox(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< bg.draw(i, j) <+< bs(ptr).draw(i+2, j+2) <+< b.draw(i, j) <+< drawArrow
  }

  def drawIntro(i:Int, j:Int, intro:Intro)(tr:TileRenderer):TileRenderer = {
    def filter(ii:Int, jj:Int) = jj >= ii + b.rows - intro.t/speed
    tr <+< bg.filterDraw(i, j, filter) <+< b.filterDraw(i, j, filter)
  }

  def drawOutro(i:Int, j:Int, outro:Outro)(tr:TileRenderer):TileRenderer = {
    def filter(ii:Int, jj:Int) = jj < ii + b.rows - outro.t/speed
    tr <+< bg.filterDraw(i, j, filter) <+< b.filterDraw(i, j, filter)
  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< (state match {
      case in@Intro(_) => drawIntro(i, j, in)
      case Reading => drawBox(i, j)
      case o@Outro(_) => drawOutro(i, j, o)
      case Done =>  (tr:TileRenderer) => tr

    })
  }
}
