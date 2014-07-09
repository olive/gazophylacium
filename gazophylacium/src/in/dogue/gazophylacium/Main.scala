package in.dogue.gazophylacium

import com.deweyvm.gleany.{GleanyInitializer, GleanyConfig, GleanyGame}
import com.deweyvm.gleany.files.PathResolver
import com.deweyvm.gleany.saving.{SettingDefaults, Settings}
import com.deweyvm.gleany.data.Point2i
import in.dogue.antiqua.Implicits._
import com.deweyvm.gleany.logging.Logger

object Main {
  def main(args: Array[String]) {

    val iconPath = "sprites/icon.gif"
    val settings = new Settings(GazophylaciumControls, new SettingDefaults() {
      val SfxVolume: Float = 0.2f
      val MusicVolume: Float = 0.2f
      val WindowSize: Point2i = Point2i(512,512)
      val DisplayMode: Int = 0
    })
    val config = new GleanyConfig(settings, "gazophylacium", iconPath.some)
    val pathResolver = new PathResolver(
      "fonts",
      "sprites",
      "sfx",
      "music",
      "shaders",
      "maps"
    )
    Logger.attachCrasher(".")
    val initializer = new GleanyInitializer(pathResolver, settings)
    GleanyGame.runGame(config, new Game(initializer))

  }
}
