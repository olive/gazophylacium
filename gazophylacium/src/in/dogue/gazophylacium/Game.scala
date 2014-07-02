package in.dogue.gazophylacium

import com.deweyvm.gleany.{Glean, GleanyInitializer, GleanyGame}
import java.util.concurrent.{TimeUnit, Callable, Executors}
import java.util
import in.dogue.gazophylacium.input.Controls


object Game {
  val RenderWidth = 512
  val RenderHeight = 512
}

class Game(initializer: GleanyInitializer) extends GleanyGame(initializer) {
  private lazy val engine = new Engine()
  override def update() {
    Controls.update()
    engine.update()
  }

  override def draw() {
    engine.draw()
  }

  override def resize(width: Int, height: Int) {
    Glean.y.settings.setWindowSize(width, height)
  }

  override def dispose() {
    val executor = Executors.newSingleThreadExecutor()
    executor.invokeAll(util.Arrays.asList(new Callable[Unit] {
      override def call(): Unit = ()
    }), 2, TimeUnit.SECONDS)
    executor.shutdown()
  }
}
