package utils

import org.apache.commons.math3.analysis.function.Logistic
import org.apache.commons.math3.complex.Complex
import processing.core.PApplet

import java.io.{BufferedWriter, File, FileWriter}
import scala.annotation.tailrec
import scala.collection.View.Empty
import scala.collection.mutable.ArrayBuffer

trait ComplexUtil {


  var i = 2
  var maxx = 0
  var maxy = 0
  var max = 0
  var sieve: Array[Boolean] = Empty.toArray

 // val ab2 = new ArrayBuffer[((Int,Complex),(Int,Int,Int),(Int,Int))]
  def mult() = {
    var i = 1
    val c = Complex.ONE.divide(i).pow(Complex.valueOf(2,0))
    val c2 = Complex.ONE.divide(i+1).pow(Complex.valueOf(2,0))
    //val c3: Complex = c.add(c2).add(c3)
    while(i<10000) {
      i+=1
    }

    //println(c3)
  }

  def dmap(value: Double, start1: Double, stop1: Double, start2: Double, stop2: Double): Double = {
    start2 + (stop2 - start2) * ((value - start1) / (stop1 - start1))
  }
  def rman(r: Double,ry: Double): LazyList[Double] = {
    val r1 = new org.apache.commons.math3.analysis.function.Sigmoid
    val l1 = new Logistic(r,1,.5,1,0,1)
    val r2 = r1.value(r)//r1.value(r)
    def bman(c1: Double, c2: Double): LazyList[Double] = {
      c1 #:: bman(c2, r1.value(c1)*c1*(1 - c2))
    }
    bman(ry,r)
  }

  def lman(c1: Complex, c2: Complex): LazyList[Complex] = {
    c1 #:: lman(c2, c1.multiply(c1).add(c2))
  }
  def lman2(c1: Complex, c2: Complex): LazyList[Complex] = {
    val c3 = Complex.valueOf(0)
    c1 #:: lman(c2, c1.multiply(c1.add(c2)).subtract(c2.multiply(c2.add(c1))))
  }
  def lman3(c1: Double, c2: Double): LazyList[Double] = {
    c1 #:: lman3(c2,1/math.pow((c1),2)+c2)
  }
  def lzc(pwr: Complex, sf: Float,iters: Int, v: Float)(implicit pa: PApplet) = {
    // Snippet for use in draw():
    //    translate(width/2f,height/2f)
    //    background(0)
    //    val sf = .4f //this is a scale factor
    //    var inc = 0f
    //    lzc(Complex.valueOf(-0.26688071118012687-inc, inc), sf, 3+its,inc.toFloat)
    //    inc += 0.0001f
    var z = 3
    var d = 1d
    val c1 = Complex.valueOf(1)
    val its = iters*2
    //pa.scale(3)

    for(d3 <- 1 to iters) {

      var cv2 = pwr
      var cv3 = cv2
      cv3 = (cv2.add(c1.divide(math.pow(d3,pwr.getReal))).add(c1.divide(math.pow(d3,pwr.getImaginary))))//(d*d)
    //  val maxx = (cv2.add(c1.divide(math.pow(d,pwr.getReal))).multiply(c1.divide(math.pow(d,pwr.getImaginary))))
      val scalx = dmap(cv3.abs(),0, pa.width,0,5).toFloat
      val scaly = dmap(cv3.getImaginary,0d,pa.height.toDouble,-2d,0d).toFloat
     // println(d,cv2,(scalx,scaly))
     // pa.point(cv3.getReal.toFloat*scalefact,cv3.getImaginary.toFloat*scalefact)
      pa.stroke(255)
      pa.line(0,0,cv3.reciprocal().getReal.toFloat*sf/2f,cv3.getImaginary.toFloat*sf/2f)// pa.line(0,0,cv3.reciprocal().getReal.toFloat*sf/2f,cv3.getImaginary.toFloat*sf/2f)
      pa.translate(cv3.reciprocal().getReal.toFloat*sf/2f,cv3.getImaginary.toFloat*sf/2f)
      pa.rotate(math.toRadians(-cv3.getArgument.toFloat+z/2d).toFloat)
      pa.line(0,0,cv3.reciprocal().conjugate().getReal.toFloat*sf/2f,-cv3.getImaginary.toFloat*sf/2f)
      pa.translate(cv3.reciprocal().conjugate().getReal.toFloat*sf/2f,-cv3.getImaginary.toFloat*sf/2f)
      pa.rotate(math.toRadians(cv3.getArgument.toFloat+z/3d).toFloat)
      cv2 = cv3
      d += d3
      z += 1
     // its = iters-20
    }
    def sst(mm: Int)(implicit pa: PApplet) = {
      val ab = new ArrayBuffer[Int]()
      maxx = pa.width
      maxy = pa.height
      max = mm
      sieve = new Array[Boolean](max + 1)
      sieve(1) = false
      var i = 2
      while ( {
        i <= max
      }) {
        sieve(i) = true
        i += 1
      }
      ab.append(i)
      ab
    }

    def su2(mx: Int): Unit = {
      var nmx = 10000
      sst(nmx)
      var ind = 1
        if (!sieve(i)) while ( {
          i * i < max && !sieve(i)
        }) i += 1
        if (sieve(i)) {
          // print(i + " ")
          var j = i * i
          while ( {
            j <= max
          }) {
            if (sieve(j)) {
              sieve(j) = false
            }
            j += i
          }
        }
        if (i * i < max) i += 1
      }

    }
  //  pa.rotate(math.toRadians(90).toFloat)
    //cv2
  def lz1(trim: Int,pwr: Double) = {
      /*
      scale(.25f)
    stroke(255,0,0)
    line(width/2f-10,0,width/2f-10,height*4)
    line(0,height/2f+1000f,width*4,height/2f+1000f)
    stroke(0,0,255)
    line(0,height/2f+((math.pow(math.Pi,2d)/6d).toFloat+10d).toFloat,width*4,height/2f+((math.pow(math.Pi,2d)/6d).toFloat+10d).toFloat)
    translate(width/2f,height/2f)
    stroke(0)
    val partialSumsLZ: ArrayBuffer[(Double,Double)] = lz1(6,2)
    println(math.pow(math.Pi,2d)/6d,height/4f+((math.pow(math.Pi,2d)/6d).toFloat+10d).toFloat,(partialSumsLZ.last._1).toFloat)
    for(p <- partialSumsLZ.indices) {
     // println((partialSumsLZ(p)._1).toFloat, (partialSumsLZ(p)._2).toFloat)
      point(0,0)
      line(0,0,(partialSumsLZ(p)._1).toFloat*10, (partialSumsLZ(p)._2).toFloat*1000)
      translate((partialSumsLZ(p)._1).toFloat*10, (partialSumsLZ(p)._2).toFloat*1000)
    }

  }
       */
    var z = 0
    val zt: ArrayBuffer[(Double,Double)] = new ArrayBuffer[(Double,Double)]()
    var d = 1d
    var ps = 0d
    var ps2 = 1d
    while(z<1) {
     ps = ps + 1/math.pow(d,pwr)//(d*d)
      d+=1
      ps2 = ps2 + 1/math.pow(d,pwr)
      zt.append((ps,ps2-ps))
      if(dtrunc(trim,ps2)-dtrunc(trim,ps) == 0) {
        z += 1

      } else {z = 0}
     // println(z,dtrunc(trim,ps))
    }
    zt
  }

  def dtrunc(digits: Int, value: Double) = {
    value.toString.take(digits).mkString.toFloat
  }
  
  def mc(cr: Double, ci: Double): (Int, Complex) = {
    val cx = new Complex(cr, ci)
    val z = Complex.valueOf(math.E)
    val li = lman2(z, cx).take(100).takeWhile(p => !p.abs().isNaN)
    val lit = li.toIndexedSeq.zipWithIndex
    val tupx = lit.last
    (tupx._2, tupx._1)
  }

  def pregen(minX: Double, maxX: Double, minY: Double, maxY: Double)
            (implicit pa: PApplet): ArrayBuffer[((Int, Complex), Int)] = {
    val ab = new ArrayBuffer[((Int, Complex), Int)]
    for (x <- 0 until pa.pixelWidth; y <- 0 until pa.pixelHeight) {
      val xr = dmap(x, 0, pa.pixelWidth, minX, maxX)
      val yr = dmap(y, 0, pa.pixelHeight, minY, maxY)
      val gc = mc(xr, yr)
//      val tempColor: (Int, Int, Int) = makeColorT(gc._2.getReal
//        , gc._2.getImaginary
//        , gc._2.abs())
      ab.append((gc, x + y * pa.pixelWidth))
   //   ab2.append((gc,tempColor,(x,y)))
    }
    ab
  }
  def pregenP(minX: Double, maxX: Double, minY: Double, maxY: Double)
            (implicit pa: PApplet): ArrayBuffer[(((Int, Complex), Int),(Int,Int))] = {
    val cellSize = 4
    val ab = new ArrayBuffer[(((Int, Complex), Int),(Int,Int))]
    for (x <- 0 until pa.pixelWidth/cellSize; y <- 0 until pa.pixelHeight/cellSize) {
      val xr = dmap(x, 0, pa.pixelWidth/cellSize, minX, maxX)
      val yr = dmap(y, 0, pa.pixelHeight/cellSize, minY, maxY)
      val gc = mc(xr, yr)
      val cellx = x*cellSize + cellSize/4
      val celly = y*cellSize + cellSize/4
      //      val tempColor: (Int, Int, Int) = makeColorT(gc._2.getReal
      //        , gc._2.getImaginary
      //        , gc._2.abs())
      ab.append(((gc, cellx + celly * pa.pixelWidth/cellSize),(cellx,celly)))
      //   ab2.append((gc,tempColor,(x,y)))
    }
    ab
  }

  def printMx(ab: ArrayBuffer[((Int, Complex), (Int, Int, Int), (Int, Int))]): Unit = {
    val st = ab.toArray
      val file = new File("/Users/cdw/Documents/fmatrix.csv")
      val bw = new BufferedWriter(new FileWriter(file))
      for (l <- st.indices by 8) {
        val st1 = st(l)._1
        val st2 = st(l)._2
        val st3 = st(l)._3
        bw.write(s"$l,${st1._1},${st1._2.getReal},${st1._2.getImaginary},${st1._2.abs()},${st2},${st3}\n")
        println(s"writing line $l of ${st.length}")
      }
      bw.close()
  }



  def generate(minX: Double, maxX: Double, minY: Double, maxY: Double)
               (implicit pa: PApplet): Unit = {
    val pgn = pregen(minX, maxX, minY, maxY)
    pgn.foreach(p => if (p._1._2.abs() < 5) {
      pa.pixels(p._2) = makeColor(p._1._1, p._1._2.getImaginary, p._1._2.abs())
    } else {
      pa.pixels(p._2) =
        makeColor(p._1._1
        , p._1._1
        , p._1._2.getImaginary)
    }
    )
    pgn.clear()
    pa.updatePixels()
  }

  def makeColor(pxi: Double, pxr: Double, pxa: Double)(implicit pa:PApplet): Int = {
    val clr1 = dmap(pxi, -pxr, pxr, 0, 255).toInt
    val clr2 = dmap(pxr, -pxi, pxi, 0, 255).toInt
    val clr3 = dmap(pxa, -pxi, pxr, 0, 255).toInt
    val c1 = if(clr1 > 255) 255 else if(clr1 < 0) 0 else clr1
    val c2 = if(clr2 > 255) 255 else if(clr2 < 0) 0 else clr2
    val c3 = if(clr3 > 255) 255 else if(clr3 < 0) 0 else clr3
    pa.color(c1, c2, c3)
  }
  def makeColorT(pxi: Double, pxr: Double, pxa: Double): (Int, Int, Int) = {
    val clr1 = dmap(pxi, -pxr, pxr, 0, 255).toInt
    val clr2 = dmap(pxr, -pxi, pxi, 0, 255).toInt
    val clr3 = dmap(pxa, -pxi, pxr, 0, 255).toInt
    val c1 = if(clr1 > 255) 255 else if(clr1 < 0) 0 else clr1
    val c2 = if(clr2 > 255) 255 else if(clr2 < 0) 0 else clr2
    val c3 = if(clr3 > 255) 255 else if(clr3 < 0) 0 else clr3
    (c1,c2,c3)
  }

  def generate2(minX: Double,maxX: Double,minY: Double,maxY: Double)(implicit pa: PApplet): Unit = {
    val pw = pa.pixelWidth.toDouble
    val ph = pa.pixelHeight.toDouble
    val cxs = (maxX-minX)/pw
    val cys = (maxY-minY)/ph
    for(x <- 0 until pw.toInt; y <- 0 until ph.toInt) {
      val cx = new Complex(x*cxs+minX,y*cys+minY)
     val setNotSet: (Complex, Int) = recal(Complex.ZERO,cx,1)
      val ec0 = setNotSet
      val ec = ec0._2
      val mc = ec0._1
      pa.pixels(x + y * pa.pixelWidth) = getColor(ec,301)
    }
    pa.updatePixels()
  }
  @tailrec
  final def recal(z: Complex, c: Complex, n: => Int): (Complex,Int) = {
    lazy val cal = n
    if(z.abs()>4d || cal > 300) {
      (c,cal)
    } else recal(z.multiply(z).add(c),c,cal+1)
  }
 // val r = recal(Complex.ZERO,cx,1+1)
 def getColor(iter: Int, max: Int)(implicit pa: PApplet): Int = {
   if (iter==max) return pa.color(0)
   val c = 2*math.log(iter)/math.log(max-3.0)
   if(c<1) pa.color((255*c).toInt, 0, 0)
   else if(c<1.5) pa.color(255, (255*(c-1)).toInt, 0)
   else pa.color(255, 255, (255*(c-2)).toInt)
 }
  
}
