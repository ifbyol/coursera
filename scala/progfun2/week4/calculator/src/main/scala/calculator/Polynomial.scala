package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(Math.pow(b(), 2) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(delta() match {
      case negative if negative < 0 => Set.empty
      case zero if zero == 0 => Set(-1 * b() / (2 * a()))
      case rest => Set((-1 * b() + Math.sqrt(rest)) / 2 * a(), (-1 * b() - Math.sqrt(rest)) / 2 * a())
    })
  }
}
