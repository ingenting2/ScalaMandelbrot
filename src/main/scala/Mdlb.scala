
import processing.core.PApplet
import processing.event.KeyEvent
import utils.ComplexUtil

class Mdlb extends PApplet with ComplexUtil {

  implicit val pa: PApplet = this
  var ex = 2d
  var sx = -1d
  var ey = 1d
  var sy = -2d
  var n = 0
  var cxx: Double = ex
  var cxn: Double = sx
  var cyx: Double = ey
  var cyn: Double = sy
  //val (min,max) = (cxn/2d,cxx/2d)
  var r = false
  var z = false
  var p = false
  //val abx: ArrayBuffer[((Int, Complex), (Int, Int, Int), (Int, Int))] = ab2

  override def settings(): Unit = {
    pixelDensity(displayDensity())
    size(300,300)
  }
  override def setup(): Unit = {
    if(p) noLoop() else loop()
    background(255)
    loadPixels()
  }


  override def draw(): Unit = {
     generateWithCenter(mouseX,mouseY)
   // println(mouseX,mouseY)
      if(p) {
       // printMx(abx)
        println("exiting")
        Thread.sleep(2000)
        exit()
      }
  }

  def generateWithCenter(mx: Int,my: Int): Unit = {
      val xp1 = dmap(mx.toDouble, 0, width, sx, ex)
      val yp1 = dmap(my.toDouble, 0, width, sy, ey)
   // println(xp1,yp1)
    reCenter((xp1,yp1))
  }
  def zoom(): Unit = {
      cxx = ex * .85d
      cxn = sx * .85d
      cyx = ey * .85d
      cyn = sy * .85d
  }

  def reCenter(mxy: (Double,Double)): Unit = {
    var rr = r
    var zz = z
    if(rr) {
      ex = cxx - mxy._1
      sx = cxn - mxy._1
      ey = cyx - mxy._2
      sy = cyn - mxy._2
      rr = !rr
    }
    if(zz) {
      zoom()
      ex = cxx
      sx = cxn
      ey = cyx
      sy = cyn
      zz = !zz
    }
    r = rr
    z = zz
    generate2(sx,ex,sy,ey)
  }

  override def handleKeyEvent(event: KeyEvent): Unit = {
    if(event.getKey == 'e') {
      println("exiting")
      Thread.sleep(2000)
      exit()
    }
    if(event.getKey == 'p') {
      p = !p
    }
    if(event.getKey == 'z') {
      z = !z
    }
    if(event.getKey == 'r') {
      r = !r
    }
    super.handleKeyEvent(event)}
}

object Mdlb extends App {
  PApplet.main("Mdlb")
}
