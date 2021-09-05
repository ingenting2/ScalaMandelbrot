import org.apache.commons.math3.complex._
import processing.core.{PApplet, PConstants}
import processing.event.KeyEvent

class Proc extends PApplet with PConstants {
  var a1 = true
  var l0 = 0f
  var ii = 0f
  var jj = -.001f
  var xx = Complex.valueOf(ii,jj)
 // var pg: PGraphics = _

  override def settings(): Unit = {
    pixelDensity = 2
    size(1200,800, "P3D")
  }

  override def setup(): Unit = {
  //  pg.loadPixels()
    // noLoop()
   // frameRate(1)
  }

  override def draw(): Unit = {

    background(0)
    val theta = ii//if(a1) x else l1

    translate(width/2f,height/2f)
    text(theta,20,20)
  //   scale(.75f)
//    val x = math.toRadians(xx.getArgument*ii).toFloat
//    val y = math.toRadians(pmouseY * height/10f + 90f).toFloat

    aTree(180,.5f,theta)

   ii+=.1f

  }

  def aTree(len: Float, lw: Float, m: Double): Unit = {
    val ln = len
    var s = len
    strokeWeight(lw)
  //  line(0,0,0,ln)
    if(len.abs > 10) {
      s *= .5f
     // sw *= .65f
      pushMatrix()
      rotate(-m.toFloat)
      stroke(255,1,255)
      strokeWeight(lw)
      line(0,0,0, s)
      translate(0,s)
      rotate(m.toFloat)
      aTree(s,lw,-m)
      popMatrix()
//      pushMatrix()
//      rotate(m.toFloat)
//      stroke(100,255,255)
//      strokeWeight(sw)
//      line(0,0,0, -s)
//      translate(0,-s)
//      aTree(s,sw,-m)
//      rotate((m).toFloat)
//      noStroke()
//      popMatrix()
//      pushMatrix()
//      stroke(255,255,255)
//      line(0,0,0, -s)
//      translate(0,-s)
//      rotate(math.toRadians(m).toFloat)
//      aTree(s,sw,m*1.01f)
//      popMatrix()
    }


  }

  override def handleKeyEvent(event: KeyEvent): Unit = {
    if(event.getKey == 'e') {
      println("exiting")
      Thread.sleep(2000)
      exit()
    }
    if(event.getKey == ' ') {a1 = !a1}
    super.handleKeyEvent(event)}
}

object Proc extends App {
  PApplet.main("Proc")
}
