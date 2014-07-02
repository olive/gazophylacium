package in.dogue.gazophylacium

import com.deweyvm.gleany.saving.{ControlName, ControlNameCollection}

class GazophylaciumControl(descriptor: String) extends ControlName {
  override def name: String = descriptor
}

object GazophylaciumControls extends ControlNameCollection[GazophylaciumControl] {
  def fromString(string: String): Option[GazophylaciumControl] = None
  def makeJoypadDefault: Map[String,String] = Map()
  def makeKeyboardDefault: Map[String,java.lang.Float] = Map()
  def values: Seq[GazophylaciumControl] = Seq()
}
