package in.dogue.gazophylacium.world

import in.dogue.antiqua.ui._
import in.dogue.gazophylacium.input.Controls
import in.dogue.antiqua.data.Code
import in.dogue.antiqua.Implicits
import Implicits._
import scala.util.Random
import in.dogue.antiqua.graphics.{Rect, Tile}
import com.deweyvm.gleany.graphics.Color
import in.dogue.gazophylacium.audio.SoundManager
import in.dogue.antiqua.graphics.Tile
import in.dogue.gazophylacium.ui._
import in.dogue.antiqua.graphics.Tile
import in.dogue.gazophylacium.ui.Intro
import in.dogue.gazophylacium.ui.Outro

object Readable {
  def create(i:Int, j:Int, cols:Int, rows:Int, read:Vector[String], page:Vector[String], r:Random) = {
    val readCs = BoxColorScheme.standard
    val pageCs = BoxColorScheme.page
    def makeRead(r:Random) = {
      val code = Vector(Code.` `, Code.` `, Code.≡, Code.`=`, Code.-).randomR(r)
      val fg = Color.DarkGrey.dim(1 + r.nextDouble)
      val bg = Color.DarkGrey.dim(3 + r.nextDouble)
      Tile(code, bg, fg)
    }

    def makePage(r:Random) = {
      val code = Vector(Code.α, Code.β, Code.Γ, Code.π, Code.Σ_u, Code.σ, Code.μ,
                        Code.τ, Code.Φ, Code.Θ, Code.Ω, Code.δ, Code.φ, Code.ϵ).randomR(r)
      val fg = Color.DarkGreen.dim(2 + r.nextDouble)
      val bg = Color.DarkGreen.dim(3 + r.nextDouble)
      Tile(code, bg, fg)
    }
    def letterSound() = SoundManager.blip.play()
    def boxSound(s:BoxState) = s match {
      case Start => ()
      case Intro(_) => SoundManager.fwip.play()
      case Reading => ()
      case Outro(_) => SoundManager.fwip.play()
      case Done => ()
    }
    val readRect = Rect.createTextured(cols, rows, makeRead, r)
    val pageRect = Rect.createTextured(cols, rows, makePage, r)
    val readBox = MessageBox.createSpace(cols, rows, read, readCs, readRect, letterSound, boxSound, r)
    val pageBox = MessageBox.createSpace(cols, rows, page, pageCs, pageRect, letterSound, boxSound, r)
    Readable(i, j, readBox, pageBox)
  }

  //def dummy(i:Int, j:Int, r:Random) = {
  //  val t0 = Vector("This is a test\nText box", "this another")
  //  val t1 = Vector("secret\nsecret\nsecret", "secret\nsecret")
  //  val t = MessageBox.createPlain(20, 10, t0, Controls.Space)
  //  val r = MessageBox.createPlain(20, 10, t1, Controls.Space)
  //  Readable(i, j, t, r)
  //}
}

case class Readable(i:Int, j:Int, read:MessageBox, page:MessageBox) {

  def isPos(p:Position) = p.x == i && p.y == j

  def standard:MessageBox = read
  def paper:MessageBox = page

}
