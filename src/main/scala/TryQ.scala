import processing.core.PApplet
import utils.SetQuads

class TryQ extends PApplet {

  var q: SetQuads = _

  override def settings(): Unit = {
    size(300,300)
  }
  override def setup(): Unit = {
    q = new SetQuads(width/2f,height/2f)
    q.box(20)
  }


  override def draw(): Unit = {
    q.mousePressed()
    q.mouseReleased()

  }

}

object TryQ extends App {
  PApplet.main("TryQ")
}
