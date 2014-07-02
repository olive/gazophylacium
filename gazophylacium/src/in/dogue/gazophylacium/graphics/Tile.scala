package in.dogue.gazophylacium.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.gazophylacium.data.Code

case class Tile(code:Code, bgColor:Color, fgColor:Color) {
  def setBg(c:Color) = copy(bgColor = c)
}
