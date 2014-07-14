package in.dogue.gazophylacium.world

import in.dogue.antiqua.data.{Rot13, Code, Array2d}
import com.deweyvm.gleany.data.Point2i
import scala.util.Random
import com.deweyvm.gleany.graphics.Color

object RoomMap {
  def load(roomCols:Int, roomRows:Int, r:Random) = {
    val cols = 4
    val rows = 4
    val items = createItems
    val last = items.last.makeItem(0,0)
    val collectable = items
    val randIndices = r.shuffle(for (i <- 0 until cols; j <- 0 until rows) yield (i, j))
    val itemIndices: Map[(Int, Int), ItemFactory] = randIndices.take(collectable.length).zip(collectable).toMap
    val machineIndex = randIndices.drop(collectable.length).head

    val specs = RoomSpec.makeSpecs(cols, rows, machineIndex)

    val rooms = specs.map { case (i, j, s) =>
      val items = itemIndices.get((i, j)).map { t => Seq(t) }.getOrElse(Seq())
      s.createRoom(cols, rows, roomCols, roomRows, Point2i(i, j), items, r)
    }
    val infos = Array2d.tabulate(cols, rows) { case (i, j) =>
      RoomInfo(Seq(last))
    }
    RoomMap(rooms, infos)
  }

  def createItems = {
    val dummy = "Dummy\n dummy dummy\n dummy dummy dunmmy"
    import Rot13._
    /***/val tbox0 = Vector("Tafasesen","Nalu denaita\nyen demiyu.","Be tanatein\nvoná mân\nadzoun ain?","Otún adzoun\naital arian.")
    /***/val tbox1 = Vector("Tanoke","Tanokérai ban yuki\nhin settérai.","Be kóimin vona ban?")
    /***/val tbox2 = Vector("Punnisen","Voná hua byaji?","Be warein voná?","Be gontoun voná?")
    /***/val tbox3 = Vector("Kêta","Punikatú ittefu\nhentere ya.","Punikatú hentere\nittefu ya.","Tafasoude ittefu\narian.")
    /***/val tbox4 = Vector("Isaya","Ikatoun ten\ndzesene fasu...","... ten aital dakubi.","Be tusammi voná?")
    /**/val tbox5 = Vector("Besori","Lemémai wan\npelelaba biloude.","Hodda be ropentoun?")

    /** */val tbox6 = Vector("Todzefu","Eta chekú.","Dadénai mantel tuni.","Hínten.")
    val box0 = Vector("Gur Culynpgrel","Gur cbffvoyvgl\nbs rgreany yvsr.","Jung jvyy lbh\nfnpevsvpr\ngb svaq vg?","Qrngu\njvyy fgvyy\npbzr sbe lbh.").map(rot13)
    val box1 = Vector("Gur Pebff","Gjb ebnqf zrrg\nnaq gura qviretr.","Juvpu cngu\njvyy lbh gnxr?").map(rot13)
    val box2 = Vector("Gur Fpnyrf","Jung vf snve?","Jung vf whfg?","Jung vf rabhtu?").map(rot13)
    val box3 = Vector("Gur Pbva","Genqr gevaxrgf\nsbe gvzr.","Genqr gvzr\nsbe gevaxrgf.","Gur gevaxrgf\njvyy bhgynfg lbh.").map(rot13)
    val box4 = Vector("Gur Gbnqfgbby","Vg znl tvir\nyvsr fhfgnvavat\nabhevfuzrag-","-be n fjvsg qrngu.","Jung qb lbh\njvfu sbe?").map(rot13)
    val box5 = Vector("Gur Fgnss","Furcureq gubfr\njub jbhyq sbyybj lbh.","Jurer jvyy\nlbh yrnq gurz?").map(rot13)
    val box6 = Vector("Gur Cntr","N oynax fyngr.","Perngr\nlbhe bja havirefr.","N qbbqyr.").map(rot13)
    Vector(
      ItemFactory(Code.¶, Color.Cyan, 0, tbox0, box0),
      ItemFactory(Code.┼, Color.Tan, 1, tbox1, box1),
      ItemFactory(Code.±, Color.Blue, 2, tbox2, box2),
      ItemFactory(Code.`.`, Color.Yellow, 3, tbox3, box3),
      ItemFactory(Code.τ, Color.Grey, 4, tbox4, box4),
      ItemFactory(Code.⌠, Color.Brown, 5, tbox5, box5),
      ItemFactory(Code.■, Color.Brown, 6, tbox6, box6)
    )
  }
}

case class RoomMap(rooms:Array2d[Room], infos:Array2d[RoomInfo]) {
  def apply(i:Int, j:Int):Option[Room] = {
    for {
      r <- rooms.getOption(i, j)
      i <- infos.getOption(i, j)
    }  yield r.load(i)
  }

  def collect(s:Seq[Item]) = {
    val newInfos = infos.map { case (i, j, it) =>
      s.foldLeft(it) { case (acc, item) => acc.collect(item) }
    }
    copy(infos=newInfos)
  }
}
