/**
  * Created by numajiri on 16/07/21.
  */

import java.io.PrintWriter;
import breeze.linalg.DenseVector;


object TMain {
  private val cnt: Int = 10000
  private val N: Int = 1000


  def main(args: Array[String]): Unit = {

    val firstState = DenseVector[Double](math.Pi / 24.0, 0)
    //init
//    TNoise.setSeed(1l)
//    val particleFirstState = DenseVector[Double](0.0, 0.0)
    val particleFirstState = firstState
    val particles: List[DenseVector[Double]] =
      initializeParticle(N, particleFirstState)
    val p = new PrintWriter("logs/result_1.csv")
    p.println("k,realX1,realX2,pfX1,pfX2")
    mainloop(0, firstState, particles, p)
    p.close()

  }

  def initializeParticle(num: Int, state: DenseVector[Double]): List[DenseVector[Double]] = {
    @annotation.tailrec
    def go(n: Int, l: List[DenseVector[Double]]): List[DenseVector[Double]]
    = n match {
      case 1 => l
      case _ if (n > 1) => {
        val tmp = state + DenseVector[Double](TNoise.getInitializeNoise, TNoise.getInitializeNoise)
        go(n - 1, tmp :: l)
      }
      case _ if (n < 0) => Nil
    }
    go(num, Nil)
  }

  def getLikelihoods(y: Double, p: List[DenseVector[Double]]): List[Double] = {
    def logLikelihoods(v: Double) =
      -(1.0 / 2.0) * (y - v) * (1.0 / TNoise.getSystemVariance) * (y - v)
    val tmp = p.map(p => logLikelihoods(p(1)))
    val coefficient = 1.0 / (math.sqrt(4.0 * math.Pi * math.Pi)
      * math.abs(TNoise.getSystemVariance))
    tmp.map(d => coefficient * math.exp(d))
  }

  def getNextParticles(is: List[Int], p: List[DenseVector[Double]]): List[DenseVector[Double]] = {
    def loop(i: List[Int], acc: List[DenseVector[Double]]): List[DenseVector[Double]]
    = i match {
      case h :: t if (h != -2) => loop(t, p.apply(h) :: acc)
      case h :: t => loop(t, p.apply(TNoise.nextInt(N)) :: acc)
      case Nil => acc
    }
    loop(is, Nil: List[DenseVector[Double]])
  }

  @annotation.tailrec
  def mainloop(k: Int,
               s: DenseVector[Double],
               l: List[DenseVector[Double]],
               p: PrintWriter): Unit
  = k match {
    case _ if (k < cnt) => {
      //predict
      val state: DenseVector[Double] = TPendulum.stateEquation(s)
      val particles = l.map(p => TPendulum.stateEquation(p))

      //filtering
      val observedState = state(1) + TNoise.getObserveNoise
      val likelihoods: List[Double] = getLikelihoods(observedState, particles)
      val sum: Double = likelihoods.foldRight(0.0)(_ + _)
      val betas: List[Double] = likelihoods.map(d => d / sum)

      val betaStairs = betas.scan(0.0)(_ + _)
      val indexes = List.fill(N)(0).map(_ =>
        betaStairs.indexWhere(_ > TNoise.nextDouble) - 1)

      val nextParticles: List[DenseVector[Double]] = getNextParticles(indexes, particles)
      val estimation: DenseVector[Double] =
        nextParticles.foldRight(DenseVector(0.0, 0.0))(_+_) / N.toDouble
      //put log
      p.println(k + "," + state(0) +
        "," + state(1) + "," + estimation(0) + "," + estimation(1))

      //update
      mainloop(k + 1, state, nextParticles, p)
    }
    case _ =>
  }
}
