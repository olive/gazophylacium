package in.dogue.gazophylacium.world.doodads

import in.dogue.antiqua.data.{Array2d, Code}
import in.dogue.antiqua.graphics.{Tile, TileRenderer, Animation}
import scala.util.Random
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.graphics.Color
import com.deweyvm.gleany.data.{Recti, Point2d}
import in.dogue.antiqua.geometry.Blob


case class MachineTile(c:Code, up:Int, down:Int, left:Int, right:Int) {
  def makeTile(bg:Color, fg:Color) = Tile(c, bg, fg)
  def isThin = up <= 1 && down <= 1 && left <= 1 && right <= 1
}

object MachineTile {
  val All = Vector(
    MachineTile(Code.│, 1, 1, 0, 0),
    MachineTile(Code.┤, 1, 1, 1, 0),
    MachineTile(Code.╡, 1, 1, 2, 0),
    MachineTile(Code.╢, 2, 2, 1, 0),
    MachineTile(Code.╖, 0, 2, 1, 0),
    MachineTile(Code.╕, 0, 1, 2, 0),
    MachineTile(Code.╣, 2, 2, 2, 0),
    MachineTile(Code.║, 2, 2, 0, 0),
    MachineTile(Code.╗, 0, 2, 2, 0),
    MachineTile(Code.╝, 2, 0, 2, 0),
    MachineTile(Code.╜, 2, 0, 1, 0),
    MachineTile(Code.╛, 1, 0, 2, 0),
    MachineTile(Code.┐, 0, 1, 1, 0),
    MachineTile(Code.└, 1, 0, 0, 1),
    MachineTile(Code.┴, 1, 0, 1, 1),
    MachineTile(Code.┬, 0, 1, 1, 1),
    MachineTile(Code.├, 1, 1, 0, 1),
    MachineTile(Code.─, 0, 0, 1, 1),
    MachineTile(Code.┼, 1, 1, 1, 1),
    MachineTile(Code.╞, 1, 1, 0, 2),
    MachineTile(Code.╟, 2, 2, 0, 1),
    MachineTile(Code.╚, 2, 0, 0, 2),
    MachineTile(Code.╔, 0, 2, 0, 2),
    MachineTile(Code.╩, 2, 0, 2, 2),
    MachineTile(Code.╦, 0, 2, 2, 2),
    MachineTile(Code.╠, 2, 2, 0, 2),
    MachineTile(Code.═, 0, 0, 2, 2),
    MachineTile(Code.╬, 2, 2, 2, 2),
    MachineTile(Code.╧, 1, 0, 2, 2),
    MachineTile(Code.╨, 2, 0, 1, 1),
    MachineTile(Code.╤, 0, 1, 2, 2),
    MachineTile(Code.╥, 0, 2, 1, 1),
    MachineTile(Code.╙, 2, 0, 0, 1),
    MachineTile(Code.╘, 1, 0, 0, 2),
    MachineTile(Code.╒, 0, 1, 0, 2),
    MachineTile(Code.╓, 0, 2, 0, 1),
    MachineTile(Code.╫, 2, 2, 1, 1),
    MachineTile(Code.╪, 1, 1, 2, 2),
    MachineTile(Code.┘, 1, 0, 1, 0),
    MachineTile(Code.┌, 0, 1, 0, 1)
  )


}

object Machine {


  def create(cols:Int, rows:Int, r:Random):Machine = {
    val blob = Blob.create(cols, rows, 6, cols*2, r)
    val mask = blob.mask


    val seed:Array2d[Option[MachineTile]] = Array2d.tabulate(cols, rows) { case (i, j) =>
      if (mask.get(i, j) && r.nextDouble < 0.1 && i > 1 && i < cols - 2 && j > 1 && j < rows - 2) {
        MachineTile.All.randomR(r).some
      } else {
        None
      }
    }

    val indices = r.shuffle(for (i <- 0 until cols; j <- 0 until rows) yield (i, j))
    val tiles:Array2d[Option[MachineTile]] = indices.foldLeft(seed) {
      case (ars, (i, j)) =>
        val t = ars.get(i, j)
        val tt = t match {
          case Some(tile) => tile.some
          case None => choose(mask, ars, i, j, r)
        }
        ars.updated(i, j, tt)
    }
    //val tiles: Array2d[Option[DevilTile]] = maskToTiles(mask)
    val flat = (tiles map { case (i, j, t) =>
      t.map{ k =>
        val tile = k.makeTile(Color.Black, Color.White)
        Animation.create(Vector((1, tile)))
      }


    }).flatten

    val soFlat = flat.map { case (i, j, o) =>
      o match {
        case Some(t) => (i, j, t).some
        case None => None
      }
    }.flatten

    Machine(soFlat, blob.span, 0)
  }




  private def maskToTiles(mask:Array2d[Boolean]):Array2d[Option[MachineTile]] = {
    mask.map { case (_, _, b) =>
      if (b) {
        MachineTile(Code.█, 0, 0, 0, 0).some
      } else {
        None
      }
    }
  }



  private def choose(mask:Array2d[Boolean], ars:Array2d[Option[MachineTile]], i:Int, j:Int, r:Random):Option[MachineTile] = {
    def getSpokes(i:Int, j:Int, f:MachineTile => Int) = {
      ars.getOption(i, j).flatten.map(f).getOrElse(-1)
    }
    def getOpen(i:Int, j:Int) = {
      mask.getOption(i, j).getOrElse(false)
    }
    val left = getSpokes(i - 1, j, _.right)
    val right = getSpokes(i + 1, j, _.left)
    val up = getSpokes(i, j - 1, _.down)
    val down = getSpokes(i, j + 1, _.up)


    val leftOpen = getOpen(i - 1, j)
    val upOpen = getOpen(i, j - 1)
    val rightOpen = getOpen(i + 1, j)
    val downOpen = getOpen(i, j + 1)
    def check(i:Int, b:Boolean, d:Int) = {
      (i == -1 || d == i) && (b || d == 0)
    }


    def f(d:MachineTile) = {
      getOpen(i, j) && (check(left, leftOpen, d.left)
        && check(right, rightOpen, d.right)
        && check(down, downOpen, d.down)
        && check(up, upOpen, d.up))
    }

    val filtered = MachineTile.All.filter(f)

    if (filtered.length == 0) {
      if (getOpen(i, j) && (leftOpen || rightOpen || upOpen || downOpen)) {

        MachineTile(Code.`#`, 0, 0, 0, 0).some
      } else {
        None
      }
    } else {
      val result = filtered.randomR(r)
      result.some
    }

  }
}

case class Machine(tiles:Seq[(Int,Int,Animation)], private val span:Recti, t:Int) {

  def getRect(i:Int, j:Int):Recti = span + Recti(i, j, 0, 0)

  def update:Machine = {
    val newTiles = tiles.smap {_.update}
    copy(tiles=newTiles,t=t+1)
  }

  private def colorPulse(t:Int):Color = {
    val dim = Math.abs(Math.sin(t / 30f)*2)
    Color.Red.dim(dim.toFloat)
  }

  def draw(i:Int, j:Int)(tr:TileRenderer):TileRenderer = {
    //val span = tr.project(getRect(i, j))
    //Engine.r.drawRect(span.x, span.y, span.width, span.height, Color.White)
    tr <++< tiles.map { case (p, q, a) =>
      a.drawWithFg(colorPulse(t), i + p, j + q) _
    }
  }

  private def isSolid(i:Int, j:Int)(p:Int, q:Int) = {
    tiles.exists{case (k, l, _) => k + i == p && l + j == q}
  }

  def toDoodad(i:Int, j:Int):Doodad[Machine] = {
    Doodad(i, j, _.draw, _.getRect, _.isSolid, _.update, this)
  }
}
