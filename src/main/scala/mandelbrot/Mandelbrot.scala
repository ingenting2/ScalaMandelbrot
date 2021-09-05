package mandelbrot

import org.apache.commons.math3.complex.Complex
import processing.core.{PApplet, PConstants, PGraphics}
import utils.ComplexUtil

class Mandelbrot extends PApplet with ComplexUtil {

  val MAX_WIDTH = 400
  val MAX_HEIGHT: Int = MAX_WIDTH
  var pg: PGraphics = new PGraphics()
  var n = 1

  override def settings(): Unit = {
    size(MAX_WIDTH,MAX_HEIGHT,PConstants.JAVA2D)
  }

  override def setup(): Unit = {
    noLoop()
    pg = new PGraphics()
  }

  override def draw(): Unit = {
    smand(1,-2)
    //lazyl(1,-1).take(20).foreach(println)

  }

  def mandelFun(x: Double, y: Double)(implicit p: PApplet) = {
    pg.setSize(MAX_WIDTH/2,MAX_HEIGHT/2)
    pg.beginDraw()
    pg.background(100)
    p.image(pg,PConstants.CENTER.toFloat,PConstants.CENTER.toFloat)
    pg.endDraw()
  }

  def smand(a: Double, b: Double) = {
    val c: Complex = new Complex(a, b)
    var z = Complex.ZERO
    //var z1 = z.pow(2).add(c)
    def lazyl(m: Complex, n: Complex): LazyList[Complex] = {
      m #:: lazyl(n, m.multiply(m).add(n))
    }
    val ll = lazyl(z,c).take(100)
    for(i <- ll.toArray.indices if !ll(i).isInfinite & !ll(i).isNaN) {
      z = ll(i)
      println(i, z)
    }
   // z
  }

  def mc(c: Complex): (Complex,Int) = {
    var z = c
   // var n = 0
    println(z.abs(),n)
    if(!z.abs().isInfinite) {
      n = n+1
      z = mc(z.pow(2).add(c))._1
    } else {
      z = Complex.ZERO
    }
    (z,n)
  }

 // def abuff()

}

object Mandelbrot extends App {
  PApplet.main("mandelbrot.Mandelbrot")
}
