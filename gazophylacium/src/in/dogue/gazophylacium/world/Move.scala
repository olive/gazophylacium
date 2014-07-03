package in.dogue.gazophylacium.world

sealed class Move(val dx:Int, val dy:Int)
case object Up extends Move(0, -1)
case object Down extends Move(0, 1)
case object Left extends Move(-1, 0)
case object Right extends Move(1, 0)
