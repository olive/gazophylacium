package in.dogue.gazophylacium.audio

import com.deweyvm.gleany.AssetLoader


object SoundManager {
  val baseVol = 0.5f



  def loadS(s:String, adj:Double) = {
    val snd = AssetLoader.loadSound(s, false)
    snd.setAdjustVolume(adj.toFloat)
    snd
  }

}
