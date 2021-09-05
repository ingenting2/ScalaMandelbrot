package utils

import processing.core.PApplet

class SetQuads(posx: Float, posy: Float) extends PApplet {
  var mx = 0
  var my = 0
  var bx: Float = posx
  var by: Float = posy
  var boxSize = 20f
  var overBox = false
  var locked = false
  var xOffset = 0.0f
  var yOffset = 0.0f



  var gboxes: Array[Array[GridBox]] = Array.ofDim[GridBox](height, width)

  class GridBox(posx: Float, posy: Float) extends SetQuads(posx,posy) {
    for (i <- 0 until height by boxSize.toInt; j <- 0 until width by boxSize.toInt) {
      gboxes(i)(j) = new GridBox(i, j)

      def boxes(): Unit = rect(bx, by, boxSize, boxSize)

      def obox(): Unit = {

        if (mx > bx - boxSize & mx < bx + boxSize & my > by - boxSize & my < by + boxSize) {
          overBox = true
          if (!locked) {
            stroke(255)
            fill(153)
          }
        }
        else {
          stroke(153)
          fill(153)
          overBox = false
        }
      }

      def run(x: Int, y: Int) = {
        mx = x
        my = y
      }
    }
  }

  override def mousePressed(): Unit = {
    if (overBox) {
      locked = true
      fill(255, 255, 255)
    }
    else locked = false
    xOffset = mx - bx
    yOffset = my - by
  }

  override def mouseDragged(): Unit = {
    if(locked) {
      bx = mx-xOffset;
      by = my-yOffset;
    }
  }
  override def mouseReleased(): Unit = {
    locked = false
  }

}
