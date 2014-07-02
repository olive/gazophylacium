package in.dogue.gazophylacium.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.gazophylacium.data.Code


object TextFactory {
  val bw = TextFactory(Color.Black, Color.White)
}
case class TextFactory(bgColor:Color, fgColor:Color) {
  def withBg(c:Color) = copy(bgColor = c)
  def withFg(c:Color) = copy(fgColor = c)

  private def makeTiles(s:String, bgColor:Color, fgColor:Color) = {
    s.map(Code.unicodeToCode).map{c => Tile(c, bgColor, fgColor)}.toVector
  }

  def create(s:String) = {
    val tiles = makeTiles(s, bgColor, fgColor)
    Text(tiles, this)
  }
}
