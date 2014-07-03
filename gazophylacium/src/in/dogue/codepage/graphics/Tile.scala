package in.dogue.codepage.graphics

import com.deweyvm.gleany.graphics.Color
import in.dogue.codepage.data.Code

case class Tile(code:Code, bgColor:Color, fgColor:Color) {
  def setBg(c:Color) = copy(bgColor = c)
}
