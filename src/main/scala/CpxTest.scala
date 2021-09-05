import processing.core.PApplet
import processing.event.{KeyEvent, MouseEvent}
import utils.SetQuads

class CpxTest extends PApplet with utils.ComplexUtil {

  implicit val pa: PApplet = this
  var p = false
  var z = false
  var r = 0
  var zc = 0
  var x1 = 0
  var y1 = 0
  var x2 = 0
  var y2 = 0
  var mevent = 0
  var setNCs: Seq[Double] = _
  var quads: SetQuads = _

  def setCorners(upperLeft: (Double, Double) = (-2d, -2d), lowerRight: (Double, Double) = (2d, 2d)): Seq[Double] = {
    List(upperLeft._1, upperLeft._2, lowerRight._1, lowerRight._2)
  }

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    size(400, 400)
  }

  override def setup(): Unit = {
    background(255)
    quads = new SetQuads(0,0)

    loadPixels()
  }

  override def draw(): Unit = {
    val setCs = setCorners()

      val gbs = quads



      // newQuads.obox(20,20)
      //generate2(setCs.head,setCs(2),setCs(1),setCs(3))
    }

    def newCalc(getX: Int, getY: Int) = {
      val setCs = setCorners()
      setNCs = setCs
      val newCenterX = dmap(getX.toDouble, 0d, width, setNCs.head, setNCs(2))
      val newCenterY = dmap(getY.toDouble, 0d, height, setNCs(1), setNCs(3))
      if (r > 0) {
        val nncs = setNCs
        setNCs = (nncs zip List(newCenterX, newCenterY, newCenterX, newCenterY)).map(p => p._1 + p._2)
      }
      if (zc > 0) {
        val newZ = setNCs.map(p => p / (2d * zc))
        setNCs = newZ
      }
      if (p) {
        setNCs = setCs
        zc = 0
        r = 0
      }

      generate2(setNCs.head, setNCs(2), setNCs(1), setNCs(3))
    }


     override def handleMouseEvent(event: MouseEvent): Unit = {
      val ev = event.getAction
      val ms = event.getMillis
      var mx = 0
      var my = 0
      var mxr = 0
      var myr = 0
      mevent = ev
      if (ev == 1) {
        mx = event.getX
        my = event.getY
      } else if (ev == 4) {
        mxr = event.getX
        myr = event.getY
        x1 = mx
        y1 = my
      } else if (ev == 2) {
        x2 = mxr
        y2 = myr
      }
      super.handleMouseEvent(event)
    }

    override def handleKeyEvent(event: KeyEvent): Unit = {
      if (event.getKey == 'e') {
        println("exiting")
        Thread.sleep(2000)
        exit()
      }
      if (event.getKey == 'p') {
        // loop()
        p = !p
        updatePixels()
        //noLoop()
      }
      if (event.getKey == 'z') {
        // loop()
        zc += 1
        updatePixels()
      }
      if (event.getKey == 'r') {
        //  loop()
        r += 1
        //      x1 = mouseX
        //      y1 = mouseY
        updatePixels()
      }
      super.handleKeyEvent(event)
    }
  }


object CpxTest extends App {
  PApplet.main("CpxTest")
}
