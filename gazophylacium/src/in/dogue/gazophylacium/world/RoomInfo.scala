package in.dogue.gazophylacium.world

case class RoomInfo(items:Seq[Item]) {
  def collect(s:Item) = {
    copy(items = items ++ Seq(s))
  }

  def contains(s:Item) = items.contains(s)
}
