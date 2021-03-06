package scalashop

import org.scalameter._
import common._
import zio.{ DefaultRuntime, ZIO }

object HorizontalBoxBlurRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 5,
    Key.exec.maxWarmupRuns -> 10,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {

    val radius = 3
    val width = 1920
    val height = 1080
    val src = new Img(width, height)
    val dst = new Img(width, height)

    val numTasks = 4
    val zioRuntime = new DefaultRuntime {}

    val seqtime = standardConfig measure {
      HorizontalBoxBlur.blur(src, dst, 0, height, radius)
    }

    val partime = standardConfig measure {
      HorizontalBoxBlur.parBlur(src, dst, numTasks, radius)
    }

    val ziotime = standardConfig measure {
      zioRuntime.unsafeRun(HorizontalBoxBlur.zioBlur(src, dst, numTasks, radius))
    }

    println(s"sequential blur time: $seqtime ms")
    println(s"fork/join blur time: $partime ms")
    println(s"zio blur time: $ziotime ms")

//    println(s"speedup: ${seqtime / partime}")

  }

}


/** A simple, trivially parallelizable computation. */
object HorizontalBoxBlur {

  /** Blurs the rows of the source image `src` into the destination image `dst`,
   *  starting with `from` and ending with `end` (non-inclusive).
   *
   *  Within each row, `blur` traverses the pixels by going from left to right.
   */
  def blur(src: Img, dst: Img, from: Int, end: Int, radius: Int): Unit = {
    var i = 0
    var j = from
    while (j < end) {
      i = 0
      while (i < src.width) {
        dst.update(i, j, boxBlurKernel(src, i, j, radius))
        i += 1
      }
      j += 1
    }
  }

  /** Blurs the rows of the source image in parallel using `numTasks` tasks.
   *
   *  Parallelization is done by stripping the source image `src` into
   *  `numTasks` separate strips, where each strip is composed of some number of
   *  rows.
   */
  def parBlur(src: Img, dst: Img, numTasks: Int, radius: Int): Unit = {
    val height = src height
    val step = math.ceil(height.toDouble / numTasks) toInt

    val startPoints = 0 until height by step
    val endPoints = 0 + step until height + step by step map { clamp(_, 0, height) }

    val strips = startPoints zip endPoints

    strips map { case (from, end) => task { blur(src, dst, from, end, radius) } } foreach { _ join() }
  }

  def zioBlur(src: Img, dst: Img, numTasks: Int, radius: Int): ZIO[Any, Nothing, List[Unit]] = {

    val height = src height
    val step = math.ceil(height.toDouble / numTasks) toInt

    val startPoints = 0 until height by step
    val endPoints = 0 + step until height + step by step map { clamp(_, 0, height) }

    val strips = startPoints zip endPoints

//    strips map { case (from, end) => task { blur(src, dst, from, end, radius) } } foreach { _ join() }

    zio.ZIO.foreachParN(numTasks)(strips) { case (from, end) => zio.ZIO succeed blur(src, dst, from, end, radius) }

  }

}
