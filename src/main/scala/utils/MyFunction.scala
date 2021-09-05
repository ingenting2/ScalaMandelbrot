package utils

import org.apache.commons.math3.analysis.UnivariateFunction
import org.apache.commons.math3.analysis.solvers.{PegasusSolver, UnivariateSolver}

class MyFunction extends UnivariateFunction {
  override def value(x: Double): Double = {
    val y = lz1(x)
    y
  }
  def lz1(dx: Double) = {
    var d = dx
    var ps = 0d
    var ps2 = 1d
    while(ps2-ps>0.1d) {
      ps = ps + 1/(dx.toDouble*dx.toDouble)
      //d+=1
      ps2 = ps2 + 1/(d*d)
      println(ps)
    }
    ps
  }

  def compute() = {
    val function: UnivariateFunction = new MyFunction// some user defined function object
    val relativeAccuracy = 1.0e-12
    val absoluteAccuracy = 1.0e-8
    val maxOrder = 5;
    val solver: UnivariateSolver = new PegasusSolver()//new BracketingNthOrderBrentSolver(relativeAccuracy, absoluteAccuracy, maxOrder);
   // solver.getEvaluations
      solver.solve(100,function,0d,3d)

    }
}
