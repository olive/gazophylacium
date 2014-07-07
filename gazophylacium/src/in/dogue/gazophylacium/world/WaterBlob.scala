package in.dogue.gazophylacium.world

import in.dogue.antiqua.data.{Code, Array2d}
import in.dogue.antiqua.graphics.{Animation, Tile}
import scala.util.Random
import in.dogue.antiqua.data
import in.dogue.antiqua.geometry.Blob
import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.data.Point2d
import in.dogue.antiqua.Implicits._
object WaterBlob {
  private def makeAnim:Animation = {
    val t0 = Tile(Code.`=`, Color.Blue, Color.White)
    val t1 = Tile(Code.≈, Color.Blue, Color.White)
    Animation.create(Vector((15, t0), (15, t1)))
  }
  def create(cols:Int, rows:Int, numPoints:Int, threshold:Int, r:Random):Blob[Option[Animation]] = {
    val blob = Blob.create(cols, rows, numPoints, threshold, r)
    val baseAnim = makeAnim
    val diagonal = (cols * cols + rows * rows).sqrt/2
    val anims = Array2d.tabulate(cols, rows) { case (i, j) =>
      if (!blob.mask.get(i, j)) {
        None
      } else {
        val dist = (Point2d(i, j) - blob.poly.centroid).magnitude
        baseAnim.dimBg((diagonal - dist)/4).some
      }
    }
    Blob(anims, blob.poly, blob.span)
  }
}


