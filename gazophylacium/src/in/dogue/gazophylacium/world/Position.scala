package in.dogue.gazophylacium.world


case class Position(x:Int, y:Int) {

  def -->(m:Move) = (m.dx + x, m.dy + y)

  def performMove(m:Move):Position = copy(x=m.dx + x, y=m.dy + y)


}

