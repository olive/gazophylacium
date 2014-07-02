package in.dogue.gazophylacium.audio

import java.io.File


object Save {
  def save(samples: Array[Short], filename:String) {
    val ints = new Array[Int](samples.length)
    for (i <- 0 until samples.length) {
      val s = samples(i)
      ints(i) = s
    }
    val sampleRate = 44100
    val wav = WavFile.newWavFile(new File(filename), 1, ints.length, 16, sampleRate)
    wav.writeFrames(ints, ints.length)
    wav.close()
  }
}


