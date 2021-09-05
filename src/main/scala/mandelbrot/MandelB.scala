package mandelbrot

//import java.awt.Color

import org.apache.commons.math3.complex.Complex
import processing.core.{PApplet, PConstants}
import processing.event.KeyEvent

class MandelB extends PApplet with PConstants {

  var ex = 2d
  var sx = -1d
  var ey = 1d
  var sy = -2d
  var n = 0
  var cxx: Double = ex
  var cxn: Double = sx
  var cyx: Double = ey
  var cyn: Double = sy
  val (min,max) = (cxn/2d,cxx/2d)
  var r = false
  var z = false
  var p = false

  override def settings(): Unit = {
    //pixelDensity = 2
   // pixelDensity(displayDensity())
    size(600,600)
  }

  override def setup(): Unit = {
    //noLoop()
    background(255)
    loadPixels()

  }

  override def draw(): Unit = {
    generateWithCenter(mouseX,mouseY)
   // redraw()
  }

  def generate2(minX: Double,maxX: Double,minY: Double,maxY: Double)={
  //  loadPixels()
    val xMin = minX
    val xMax =  maxX
    val yMin = minY
    val yMax =  maxY
    val cx=(xMax-xMin)/width
    val cy=(yMax-yMin)/height
    var mi = 1
    for(x <- 0 until height; y <- 0 until width) {
      val c = new Complex(xMin+x*cx,yMin+y*cy)
      val iter = itMandel(c, 100, 2)
      //println(iter)
      if(iter<mi) {
        pixels(x+y*width) = color(getColor(iter, 100))
      } else pixels(x+y*width) = color(getColor(100, 100))
      mi += 1
    }

    updatePixels()
   // redraw()
  }

  def itMandel(c: Complex, imax: Int, bailout: Int): Int = {
    var z = Complex.ZERO
    var z2 = Complex.ZERO
    var m = 0
    for(i <- 0 until imax) {
      z = z2.multiply(z2).add(c)
      z2 = z
     // println(z2.abs())
      if(z2.abs() < bailout) m+=1 else m-=1
    }
    math.abs(m)
  }
  def dmap(value: Double, start1: Double, stop1: Double, start2: Double, stop2: Double): Double = {
    start2 + (stop2 - start2) * ((value - start1) / (stop1 - start1))
  }

  def getColor(iter: Int, max: Int): Int = {
    if (iter==max) return color(0)
    val c = 2*math.log(iter)/math.log(max-3.0)
    if(c<1) color((255*c).toInt, 0, 0)
    else if(c<2) color(255, (255*(c-1)).toInt, 0)
    else color(255, 255, (255*(c-2)).toInt)
  }
  def generateWithCenter(mx: Int,my: Int): Unit = {
    val xp1 = dmap(mx.toDouble, 0d, width.toDouble, sx, ex)
    val yp1 = dmap(my.toDouble, 0d, height.toDouble, sy, ey)
    // println(xp1,yp1)
    reCenter((xp1,yp1))
  }
  def zoom(): Unit = {
    cxx = ex * .55d
    cxn = sx * .55d
    cyx = ey * .55d
    cyn = sy * .55d
  }

  def reCenter(mxy: (Double,Double)): Unit = {
    var rr = r
    var zz = z
    if(rr) {
      ex = cxx + mxy._1
      sx = cxn + mxy._1
      ey = cyx + mxy._2
      sy = cyn + mxy._2
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

object MandelB extends App {
  PApplet.main("mandelbrot.MandelB")
}
