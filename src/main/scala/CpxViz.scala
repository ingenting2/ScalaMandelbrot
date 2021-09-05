import processing.core.PConstants.{CLOSE, POLYGON}
import processing.core.{PApplet, PConstants, PShape, PVector}
import processing.opengl.PGraphics3D
import utils.ComplexUtil

import scala.collection.mutable.ArrayBuffer

class CpxViz extends PApplet with ComplexUtil with PConstants {

  implicit val pa: PApplet = this
  var its = 0
  var shape: PShape = _
//  var inc = 0.0001d
//  var ii = 1d+inc
//  var jj = -5d
//  var cpx: Complex = Complex.valueOf(ii, jj)

  def rend(): PGraphics3D = new PGraphics3D


  override def settings(): Unit = {
    val renderer: Unit = rend().setPath(this.toString)

    pixelDensity(displayDensity())
    size(1100, 800,PConstants.P3D)
  }

  override def setup(): Unit = {
    noLoop()
    shapeVertices(100,3,POLYGON,0,0)
  //  super.setup()
   // frameRate(8)
  }

  //val newPS: ArrayBuffer[Double] = partialSumsLZ.scanLeft(partialSumsLZ(0)._2)( - _)
  override def draw(): Unit = {
    translate(width/2f,height/2f)
    shape(shape)
  }
  def radians(angle: Double): Float = {
    math.toRadians(angle).toFloat
  }
  def aTree(len: Float, lw: Float, m: Float): Unit = {
    val ln = len
    var s = len
    var sw = lw
    stroke(255)
    strokeWeight(lw)

    if(len.abs > 2) {
      s /= 3f
     // sw *= .66f

      pushMatrix()
    //  rotate(radians(180))
      for(i<-1 to 9) {
        rotate(math.toRadians(m).toFloat)
        line(0, 0, s, 0)
        translate(s, 0)
        rotate(math.toRadians(m).toFloat)
      }
      aTree(s,sw,m)
      popMatrix()
    }
  }

  def circumradius(l: Float, s: Int): Float = {
    val c = l / (2 * math.sin(math.Pi/s.toFloat))
    c.toFloat
  }

  def internalAngle(sides: Float): Float = math.toRadians(360/sides).toFloat

  def vecA(length: Float, sides: Int, x: Float, y: Float): Array[PVector] = {
    val cr = circumradius(length,sides)
    val ab = new ArrayBuffer[PVector]()
    val npv = new PVector(cr,0)
    npv.rotate(radians(-60))
    for(_ <- 1 to sides) {
      val nv = new PVector()
      npv.rotate(internalAngle(sides))
      nv.set(npv)
      ab.append(nv)
    }
    // ab.append(new PVector(0f,0f))
    ab.toArray
  }
  def shapeVertices(l: Float, s: Int, k: Int, x: Float, y: Float): PShape = {
    val ln = l
    val sd = s
    val kd = k
    val nv: Array[PVector] = vecA(ln,sd,x,y)
    //nv copyToArray (vxy,0)
    shape = createShape()
    shape.beginShape(kd)
    for(i <- nv.indices) {
      shape.vertex(nv(i).x, nv(i).y)
    }
    shape.endShape(CLOSE)
    //  shape.setStroke(color(0))
    //if(sd == 4) shape.rotate(math.toRadians(-45d).toFloat)
    shape
  }



}

object CpxViz extends App {
  PApplet.main("CpxViz")
}
