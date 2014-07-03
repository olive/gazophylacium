package in.dogue.gazophylacium.world

class Room(cols:Int, rows:Int) {

  def isSolid(i:Int, j:Int) = {
    i > cols - 1 || i < 0 || j > rows - 1 || j < 0
  }


  def checkMove(p:Position, m:Move):Position = {
    if ((isSolid _).tupled(p --> m)) {
      p
    } else {
      p.performMove(m)
    }

  }
}
