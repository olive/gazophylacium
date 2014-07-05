package in.dogue.antiqua.graphics

object Animation {
  def create(frames:Vector[(Int,Tile)]) = {
    Animation(frames, 0, 0)
  }
}

case class Animation(frames:Vector[(Int,Tile)], ptr:Int, t:Int) {
  def update:Animation = {
    val (newT, newPtr) = if (t > frames(ptr)._1) {
      (0, (ptr + 1 + frames.length) % frames.length)
    } else {
      (t+1, ptr)
    }
    copy(t=newT, ptr=newPtr)
  }

  def getTile = frames(ptr)._2
  def getCode = getTile.code
  def getFg = getTile.fgColor

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    tr <+ (i, j, getTile)
  }
}
