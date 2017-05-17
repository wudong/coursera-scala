package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal {
      val bb = b()
      val cc = c()
      val aa = a()

      (bb * bb) - 4 * aa * cc
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {

      delta()  match {
        case x if x < 0 => Set()
        case x if x >= 0 => {
          val det= math.sqrt(x)
          val bb = b()
          val aa = a()
          val v1 = (det - bb ) / (2*aa)
          val v2 = (-det - bb ) / (2*aa)
          Set(v1, v2)
        }
      }
    }
  }
}
