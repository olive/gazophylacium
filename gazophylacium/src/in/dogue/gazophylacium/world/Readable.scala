package in.dogue.gazophylacium.world

import in.dogue.antiqua.ui.MessageBox
import in.dogue.gazophylacium.input.Controls

object Readable {
  def create(i:Int, j:Int, read:Vector[String], page:Vector[String]) = {
    val readBox = MessageBox.createSpace(i, j, read)
    val pageBox = MessageBox.createSpace(i, j, page)
    Readable(i, j, readBox, pageBox)
  }

  def dummy(i:Int, j:Int) = {
    val t = MessageBox.create(20, 10, Vector("This is a test\nText box", "this another"), Controls.Space)
    val r = MessageBox.create(20, 10, Vector("secret\nsecret\nsecret", "secret\nsecret"), Controls.Space)
    Readable(i, j, t, r)
  }
}

case class Readable(i:Int, j:Int, read:MessageBox, page:MessageBox) {

  def isPos(p:Position) = p.x == i && p.y == j

  def standard:MessageBox = read
  def paper:MessageBox = page

}
