package reductions

import common._
import org.scalameter._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)


    println("======== sequential ========")
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    println("======== parallel money threshold ========")
    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    println("======== parallel total coins threshold ========")
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    println("======== parallel combined threshold ========")
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /**
    * When true => sequential
    */
  type Threshold = (Int, List[Int]) => Boolean

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = (money, coins) match {
    case (0, _) => 1
    case (_, List()) => 0
    case (m, _) if m < 0 => 0
    case (m, c :: cs) => countChange(m - c, c :: cs) + countChange(m, cs)
  }

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    def go = (money, coins) match {
      case (0, _) => 1
      case (_, List()) => 0
      case (m, _) if m < 0 => 0
      case (m, c :: cs) =>
        val (left, right) = parallel(parCountChange(m - c, c :: cs, threshold), parCountChange(m, cs, threshold))
        left + right
    }

    if (threshold(money, coins)) countChange(money, coins)
    else go
  }

  def noThreshold: Threshold = (_, _) => false

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = (money, _) => money * 100 / startingMoney <= 80

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = (_, coins) => coins.length * 100 / totalCoins <= 80

  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = (money, coins) =>
    moneyThreshold(startingMoney)(money, coins) && totalCoinsThreshold(allCoins length)(money, coins)

}
