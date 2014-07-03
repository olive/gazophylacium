package in.dogue.antiqua.ui

import in.dogue.antiqua.graphics._
import in.dogue.antiqua.graphics.Text
import com.deweyvm.gleany.graphics.Color

object Line {
  //def create(s:String)
}
case class Line(v:Text) {
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< v.draw(i, j)
  }
}


object TextBox {
  def create(cols:Int, rows:Int, xs:Vector[String]):TextBox = {
    val f = TextFactory(Color.Black, Color.White)
    val lines = xs map {x => Line(f.create(x))}
    TextBox(f, lines)
  }
}

case class TextBox(f:TextFactory, lines:Vector[Line]) {
  def update:TextBox = this
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <++< (for (k <- 0 until lines.length) yield {
      lines(k).draw(i, j+k) _
    })
  }

  def isFinished:Boolean = false
}

object MessageBox {
  def create(cols:Int, rows:Int, xs:Vector[String]):MessageBox = {
    val f = TextFactory(Color.Black, Color.White)
    val b = Border.standard(cols, rows)
    val boxes = xs map {_.split("\n").toVector}
    val all = boxes map {b => TextBox(f, b.map(bb => Line(f.create(bb))))}
    MessageBox(all, b, 0)

  }
}
case class MessageBox(bs:Vector[TextBox], b:Border, ptr:Int) {
  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+< bs(ptr).draw(i+2, j+2) <+< b.draw(i, j)
  }
}
