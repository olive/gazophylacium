package in.dogue.gazophylacium.world

import in.dogue.antiqua.graphics.{Animation, TileRenderer}
import in.dogue.antiqua.Implicits._
import in.dogue.antiqua.data.{Code, Array2d}
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import com.deweyvm.gleany.data.{Point2d, Recti, Point2i}
import in.dogue.gazophylacium.data._
import in.dogue.gazophylacium.world.doodads._
import in.dogue.antiqua.graphics.Tile
import scala.Some
import in.dogue.gazophylacium.world.doodads.Doodad
import in.dogue.antiqua.Implicits
import Implicits._
import in.dogue.gazophylacium.audio.SoundManager
import in.dogue.gazophylacium.ui.MessageBox

object RoomSpec {
  def makeSpecs(worldCols:Int, worldRows:Int, machineIndex:(Int,Int)):Array2d[RoomSpec] = {
    Array2d.tabulate(worldCols, worldRows) { case (i, j) =>
      RoomSpec(false, (i, j) == machineIndex)
    }
  }
}
case class RoomSpec(hasWater:Boolean, hasMachine:Boolean) {
  def createRoom(worldCols:Int, worldRows:Int, cols:Int, rows:Int, index:Point2i, items:Seq[ItemFactory], r:Random):Room = {
    Room.createRandom(this, worldCols, worldRows, cols, rows, index, items, r)
  }
}

object Room {

  object TerrainScheme {
    def create(codes:IndexedSeq[(Int, Code)], bg:ColorScheme, fg:ColorScheme) = {
      TerrainScheme(codes.expand, bg, fg)
    }
    val Grassy = {
      val codes = Vector(
        (1, Code.`'`), (1, Code.`,`), (1, Code.`.`), (1, Code.`"`), (5, Code.` `)
      )
      val bg = Vector(
        (1, Color.GrossGreen)
      )
      val fg = Vector(
        (1, Color.GrossGreen), (1, Color.DarkGreen)
      )
      create(codes, ColorScheme(bg.expand, 3, 1), ColorScheme(fg.expand, 1, 1))
    }

    val Burnt = {
      val codes = Vector(
        (1, Code.`'`), (1, Code.`,`), (1, Code.`.`), (1, Code.`"`), (5, Code.` `)
      )
      val darkBrown = Color.Tan.dim(4)
      val bg = Vector(
        (1, darkBrown)
      )
      val fg = Vector(
        (1, darkBrown), (1, Color.Brown)
      )
      create(codes, ColorScheme(bg.expand, 3, 1), ColorScheme(fg.expand, 1, 1))
    }
  }

  case class ColorScheme(colors:IndexedSeq[Color], dimBase:Double, dimAmt:Double) {
    def getColor(r:Random) = colors.randomR(r).dim((dimBase + dimAmt*r.nextFloat()).toFloat)
  }

  case class TerrainScheme(codes:IndexedSeq[Code], bg:ColorScheme, fg:ColorScheme) {
    def getBg(r:Random) = bg.getColor(r)
    def getFg(r:Random) = fg.getColor(r)
    def getCode(r:Random) = codes.randomR(r)
  }

  private def generateTerrain(biome:Biome, cols:Int, rows:Int, r:Random) = {
    val hasWater = r.nextDouble < biome.waterProb
    val tiles = Array2d.tabulate(cols, rows) { case (i, j) =>
      val code = biome.scheme.getCode(r)
      val bg = biome.scheme.getBg(r)
      val fg = biome.scheme.getFg(r)
      Animation.singleton(Tile(code, bg, fg))
    }

    val wcols = 20
    val wrows = 20
    val water = WaterBlob.create(wcols, wrows, 12, wcols*2, r)
    val wx = 10
    val wy = 10
    val ttiles =
      if (hasWater) {
        tiles.map {
          case (i, j, tile) =>
            water.mask.getOption(i - wx, j - wy).flatten match {
              case Some(w) => w
              case None => tile
            }
        }
      } else {
        tiles
      }


    val solid = tiles.map { case (i, j, tile) =>
      water.mask.getOption(i - wx, j - wy).flatten match {
        case Some(w) => hasWater
        case None => false
      }
    }
    Terrain(ttiles.flatten, solid)

  }



  private def makeDoodads(dds:Seq[Doodad[_]], biome:Biome, terrain:Terrain, cols:Int, rows:Int, hasMachine:Boolean, r:Random):(Seq[Doodad[_]], Seq[Doodad[_]]) = {
    val numTrees = 50
    //val trunkColor = Color.Tan.dim(2)
    //val leafColor = Color.Brown.dim(2)
    val makeTrees = biome.treeTypes

    val allTrees = (for (i <- 0 until numTrees) yield {
      makeTrees.randomR(r)(r, r.nextInt(cols), r.nextInt(rows))
    }).toVector

    val rawTrees = ArrayBuffer[Doodad[_]]()

    def oob(rect:Recti) = {
      rect.x < 0 || rect.right >= cols - 1 || rect.y < 0 || rect.bottom >= rows - 1
    }

    def isUnwalkable(rect:Recti) = {
      val ij = for (i <- rect.x until rect.x + rect.width;
                    j <- rect.y until rect.y + rect.height) yield (i, j)
      ij.exists{ case (i, j) => terrain.isSolid(i, j) }
    }
    val machines = if (hasMachine) {
        val m = Machine.create(10, 10, r).toDoodad(r.nextInt(cols - 10), r.nextInt(rows - 10))
        Vector(m)
      } else {
        Vector()
      }
    val d = machines ++ dds
    for (i <- 0 until numTrees) {
      var found = false
      val t0 = allTrees(i)
      val t0r = t0.getRect
      val water = isUnwalkable(t0r)
      if (d.exists{ m => t0r.intersects(m.getRect) } || water) {
        found = true
      }
      if (!found) {
        for (k <- i + 1 until numTrees) {

          val t1 = allTrees(k)
          val isOob = oob(t0r)
          val intersects = t0r.intersects(t1.getRect)

          if (intersects || isOob) {
            found = true
          }
        }
      }
      if (!found) {
        rawTrees += allTrees(i)
      }
      ()
    }
    (rawTrees ++ d, machines)
  }


  private def getCritters(biome:Biome, cols:Int, rows:Int, r:Random) = {
    val buff = 6
    val cx = buff + r.nextInt(cols - 2*buff)
    val cy = buff + r.nextInt(rows - 2*buff)
    val c = biome.critters.randomR(r)(cx, cy, 20, r)
    Seq(c)
  }

  private def getItems(cols:Int, rows:Int, terrain:Terrain, items:Seq[ItemFactory], r:Random):(Seq[Item], Seq[Doodad[_]], Seq[Readable]) = {
    var found = false
    var pos = Point2i(0,0)
    val buff = 4
    while (!found) {
      pos = Point2i(r.nextInt(cols - buff) + buff/2, r.nextInt(rows - buff) + buff/2)
      if (terrain.isSolid(pos.x, pos.y)){

      } else {
        found = true
      }
    }


    val is = items.map {_.makeItem(pos.x, pos.y)}
    val ds = items.map { is => Pedestal.create.toDoodad(pos.x-1,pos.y-2)}
    val rs = items.map { _.makeReadable(pos.x, pos.y-1, 25, 10, r) }
    (is, ds, rs)
  }

  def createRandom(spec:RoomSpec, worldCols:Int, worldRows:Int, cols:Int, rows:Int, index:Point2i, is:Seq[ItemFactory], r:Random) = {
    val biome = Vector(Biome.Forest, Biome.BurntForest).randomR(r)
    val terrain = generateTerrain(biome, cols, rows, r)
    val (items, ids, irs) = getItems(cols, rows, terrain, is, r)
    val (doodads, ms) = makeDoodads(ids, biome, terrain, rows, cols, spec.hasMachine, r)
    val readables = irs
    val critters = getCritters(biome, cols, rows, r)
    Room(worldCols, worldRows, cols, rows, index, terrain, readables, doodads.toVector, critters, items, ms)
  }
}

case class Room(worldCols:Int, worldRows:Int, cols:Int, rows:Int, index:Point2i, terrain:Terrain, rds:Seq[Readable], ts:Seq[Doodad[_]], cs:Seq[Critter], is:Seq[Item], ms:Seq[Doodad[_]]) {

  def load(info:RoomInfo) = {
    copy(is=is.filter{it => !info.contains(it)})
  }

  def getMachinePos:Option[Recti] = {
    val found = ms.headOption
    found match {
      case Some(m) => m.getRect.some
      case None => None
    }
  }

  def getOob(p:Position):Option[Direction] = {
    val i = p.x
    val j = p.y
    if (i > cols - 1) {
      Right.some
    } else if (i < 0) {
      Left.some
    } else if (j > rows - 1) {
      Down.some
    } else if (j < 0) {
      Up.some
    } else {
      None
    }
  }

  def update:Room = {
    val newCs = cs.map{_.update}
    val newTs = ts.map{_.update}
    val newIs = is.map{_.update}
    val newTerrain = terrain.update
    copy(cs=newCs, ts=newTs, is=newIs, terrain=newTerrain)
  }

  def checkItem(i:Int, j:Int):(Seq[Item],Room) = {
    def isItem(it:Item, i:Int, j:Int) = it.i == i && it.j == j
    val found = is.find{it => isItem(it, i, j)}
    if (found.isDefined) {
      val newItems = is.filter{it => !isItem(it, i, j)}
      SoundManager.collect.play()
      (Seq(found).flatten, copy(is=newItems))
    } else {
      (Seq(), this)
    }
  }

  def checkRead(i:Int, j:Int, paperOut:Boolean):Option[MessageBox] = {
    val boxes = for ((p, q) <- Seq((i + 1, j), (i - 1, j), (i, j - 1), (i, j + 1))) yield {
      rds.find{_.isPos(Position.create(p, q))} match {
        case Some(rd) =>
          if (paperOut) {
            rd.paper.some
          } else {
            rd.standard.some
          }
        case None => None

      }
    }
    boxes.flatten.headOption
  }

  private def solidReadable(p:Position) = {
    rds.exists{_.isPos(p)}
  }

  private def solidTree(p:Position) = {
    ts.exists {t =>
      t.isSolid(p.x, p.y)
    }
  }

  private def isWorldOob(p:Position) = {
    val i = p.x
    val j = p.y
    (
      (i < 0 && index.x == 0)
      || (i > cols - 1 && index.x == worldCols - 1)
      || (j < 0 && index.y == 0)
      || (j > rows - 1 && index.y == worldRows - 1)
      )
  }

  private def isRoomOob(p:Position) = {
    val i = p.x
    val j = p.y
    i < 0 || i > cols - 1 || j < 0 || j > rows - 1
  }

  private def isSolid(p:Position):Boolean = {
    isWorldOob(p) || solidReadable(p) || solidTree(p) || terrain.isSolid(p.x, p.y)
  }

  private def isStuck(p:Position):Boolean = {
    val poses = Vector(p --> Right, p --> Up, p --> Down, p --> Left)
    poses.forall(isSolid) || isSolid(p)
  }

  def checkMove(p:Position, m:Direction):Position = {
    val newPos = p --> m
    if (isSolid(newPos)) {
      SoundManager.oof.play()
      p.copy(d=m)
    } else {
      SoundManager.step.play()
      p.performMove(m)
    }
  }

  def checkStuck(p:Position):Position = {
    var pp = p
    while (isStuck(pp)) {
      pp = pp --> p.d
      if (isRoomOob(pp)) {
        for (i <- 0 until cols; j <- 0 until rows) {
          val pos = Position.create(i, j)
          if (!isStuck(pos)) {
            return pos
          }
        }
      }
    }
    pp
  }

  def draw(tr:TileRenderer):TileRenderer = {
    tr.<+<(terrain.draw)
      .<++<(ts.map {_.drawBg _})
      .<++<(cs.map {_.draw _})
  }

  def drawFg(tr:TileRenderer):TileRenderer = {
    tr <++< ts.map {_.drawFg _} <++< is.map {_.draw _}
  }
}
