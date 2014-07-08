package in.dogue.gazophylacium.world

import in.dogue.gazophylacium.world.Room.TerrainScheme
import scala.util.Random
import in.dogue.gazophylacium.world.doodads.{Tree, DeadTree, Stump, Doodad}
import com.deweyvm.gleany.graphics.Color


object Biome {
  val Forest = {
    val trunkColor = Tree.getTrunkColor
    val leafColor = Tree.getLeafColor
    val makeTrees = Vector(
      (r:Random, i:Int, j:Int) => Tree(trunkColor, leafColor)(r).toDoodad(i, j)
    )
    val color = TerrainScheme.Grassy
    val critters = Vector(
      Critter.createBird _,
      Critter.createFrog _,
      Critter.createSnake _,
      Critter.createDragonfly _
    )
    Biome(color, makeTrees, 0.2, critters)
  }

  val BurntForest = {
    val trunkColor = Color.Tan.dim(2)
    val makeTrees = Vector(
      (r:Random, i:Int, j:Int) => Stump(trunkColor)(r).toDoodad(i, j),
      (r:Random, i:Int, j:Int) => DeadTree(trunkColor)(r).toDoodad(i, j)

    )
    val color = TerrainScheme.Burnt
    val critters = Vector(
      Critter.createBug _,
      Critter.createLizard _,
      Critter.createBat _,
      Critter.createDino _
    )

    Biome(color, makeTrees, 0, critters)
  }
}
case class Biome(scheme:TerrainScheme,
                 treeTypes:IndexedSeq[(Random,Int,Int)=>Doodad[_]],
                 waterProb:Double,
                 critters:IndexedSeq[(Int,Int,Int,Random)=>Critter]) {

}
