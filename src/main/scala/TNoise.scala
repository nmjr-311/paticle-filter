import scala.util.Random

/**
  * Created by numajiri on 16/07/22.
  */
object TNoise {
  private val rand: Random = new Random()
  private val initializeVar = 0.2*0.2
  private val vVar = 0.2 * 0.2
  private val wVar = 0.1*0.1

  def setSeed(seed: Long): Unit = rand.setSeed(seed)
  private def getNoise(v: Double): Double = rand.nextGaussian() * math.sqrt(v)

  def getSystemNoise: Double = getNoise(vVar)
  def getObserveNoise: Double = getNoise(wVar)
  def getInitializeNoise: Double = getNoise(initializeVar)
  def getSystemVariance: Double = vVar
  def nextDouble:Double = rand.nextDouble()
  def nextInt(n: Int): Int = rand.nextInt(n)

}
