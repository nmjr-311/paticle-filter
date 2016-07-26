import breeze.linalg.DenseVector

/**
  * Created by numajiri on 16/07/23.
  */
object TPendulum {
  private val g: Double = 9.8
  private val m: Double = 1.0
  private val l: Double = 2.0
  private val T: Double = 1e-3

  def stateEquation(v: DenseVector[Double]) = {
    val newState = DenseVector(v(1), -(g/ l) * math.sin(v(0))) +
                    DenseVector(0.0, -1.0 / (m * l)) * TNoise.getSystemNoise
    v + newState * T
  }
//
//  def stateEquationWithNoise(v: DenseVector[Double]) = {
//    val noise = DenseVector(0.0, -1.0 / (m * l)) * TNoise.getSystemNoise
//    stateEquation(v) + noise * T
//  }


}
