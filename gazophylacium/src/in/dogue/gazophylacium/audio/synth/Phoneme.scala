package in.dogue.gazophylacium.audio.synth

import com.deweyvm.gleany.GleanyMath


object Helpers {
  def cutLevel(x: Double, lvl: Double): Double = {
    GleanyMath.clamp(x, -lvl, lvl)
  }
}

object Phoneme {
  def apply(f0:Int, f1:Int, f2:Int, w0:Int, w1:Int, w2:Int, shape:Shape):Phoneme = {
    Phoneme(Array[java.lang.Integer](f0, f1, f2),
            Array[java.lang.Integer](w0, w1, w2),
            shape)
  }

  def find(c:Char):Phoneme = {
    phonemes.get(c).getOrElse(Phoneme(0,0,0,0,0,0,Shape(3,0,false,false)))
  }

  val phonemes = Map(
    'o' -> Phoneme( 12,  15,   0,  10,  10,   0, Shape(3,  6, false, false)),
    'i' -> Phoneme(  5,  56,   0,  10,  10,   0, Shape(3,  3, false, false)),
    'j' -> Phoneme(  5,  56,   0,  10,  10,   0, Shape(1,  3, false, false)),
    'u' -> Phoneme(  5,  14,   0,  10,  10,   0, Shape(3,  3, false, false)),
    'a' -> Phoneme( 18,  30,   0,  10,  10,   0, Shape(3, 15, false, false)),
    'e' -> Phoneme( 14,  50,   0,  10,  10,   0, Shape(3, 15, false, false)),
    'E' -> Phoneme( 20,  40,   0,  10,  10,   0, Shape(3, 12, false, false)),
    'w' -> Phoneme(  3,  14,   0,  10,  10,   0, Shape(3,  1, false, false)),
    'v' -> Phoneme(  2,  20,   0,  20,  10,   0, Shape(3,  3, false, false)),
    'T' -> Phoneme(  2,  20,   0,  40,   1,   0, Shape(3,  5, false, false)),
    'z' -> Phoneme(  5,  28,  80,  10,   5,  10, Shape(3,  3, false, false)),
    'Z' -> Phoneme(  4,  30,  60,  50,   1,   5, Shape(3,  5, false, false)),
    'b' -> Phoneme(  4,   0,   0,  10,   0,   0, Shape(1,  2, false, false)),
    'd' -> Phoneme(  4,  40,  80,  10,  10,  10, Shape(1,  2, false, false)),
    'm' -> Phoneme(  4,  20,   0,  10,  10,   0, Shape(3,  2, false, false)),
    'n' -> Phoneme(  4,  40,   0,  10,  10,   0, Shape(3,  2, false, false)),
    'r' -> Phoneme(  3,  10,  20,  30,   8,   1, Shape(3,  3, false, false)),
    'l' -> Phoneme(  8,  20,   0,  10,  10,   0, Shape(3,  5, false, false)),
    'g' -> Phoneme(  2,  10,  26,  15,   5,   2, Shape(2,  1, false, false)),
    'f' -> Phoneme(  8,  20,  34,  10,  10,  10, Shape(3,  4,  true, false)),
    'h' -> Phoneme( 22,  26,  32,  30,  10,  30, Shape(1, 10,  true, false)),
    's' -> Phoneme( 80, 110,   0,  80,  40,   0, Shape(3,  5,  true, false)),
    'S' -> Phoneme( 20,  30,   0, 100, 100,   0, Shape(3, 10,  true, false)),
    'p' -> Phoneme(  4,  10,  20,   5,  10,  10, Shape(1,  2,  true,  true)),
    't' -> Phoneme(  4,  20,  40,  10,  20,   5, Shape(1,  3,  true,  true)),
    'k' -> Phoneme( 20,  80,   0,  10,  10,   0, Shape(1,  3,  true,  true))

  )
}
case class Phoneme(f:IndexedSeq[java.lang.Integer], w:IndexedSeq[java.lang.Integer], shape:Shape)

case class Shape(len:Int, amp:Int, osc:Boolean, plosive:Boolean)
