package in.dogue.gazophylacium.world

sealed class Direction(val dx:Int, val dy:Int)
case object Up extends Direction(0, -1)
case object Down extends Direction(0, 1)
case object Left extends Direction(-1, 0)
case object Right extends Direction(1, 0)
