import barneshut.conctrees._
import common._

package object barneshut {

  class Boundaries {
    var minX = Float.MaxValue

    var minY = Float.MaxValue

    var maxX = Float.MinValue

    var maxY = Float.MinValue

    def width = maxX - minX

    def height = maxY - minY

    def size = math.max(width, height)

    def centerX = minX + width / 2

    def centerY = minY + height / 2

    override def toString = s"Boundaries($minX, $minY, $maxX, $maxY)"
  }

  sealed abstract class Quad {
    def massX: Float

    def massY: Float

    def mass: Float

    def centerX: Float

    def centerY: Float

    def size: Float

    def total: Int

    def insert(b: Body): Quad
  }

  case class Empty(centerX: Float, centerY: Float, size: Float) extends Quad {
    def massX: Float = centerX
    def massY: Float = centerY
    def mass: Float = 0
    def total: Int = 0
    def insert(b: Body): Quad = Leaf(centerX, centerY, size, b :: Nil)
  }

  /**
    * mass = m_B + m_C + m_D + m_E
    * massX = (m_B * x_B + m_C * x_C + m_D * x_D + m_E * x_E) / mass
    * massY = (m_B * y_B + m_C * y_C + m_D * y_D + m_E * y_E) / mass
    */
  case class Fork(
    nw: Quad, ne: Quad, sw: Quad, se: Quad
  ) extends Quad {

    private val quadrantList = List(nw, ne, sw, se)

    val centerX: Float = nw.centerX + (ne.centerX - nw.centerX) / 2
    val centerY: Float = ne.centerY + (se.centerY - ne.centerY) / 2
    val size: Float = nw.size + ne.size
    val mass: Float = nw.mass + ne.mass + sw.mass + se.mass
    val massX: Float = if (mass == 0f) centerX else quadrantList.map(q => q.massX * q.mass).sum / mass
    val massY: Float = if (mass == 0f) centerY else quadrantList.map(q => q.massY * q.mass).sum / mass
    val total: Int = nw.total + ne.total + sw.total + se.total

    def insert(b: Body): Fork = {

      val nwDist = distance(nw.centerX, nw.centerY, b.x, b.y)
      val neDist = distance(ne.centerX, ne.centerY, b.x, b.y)
      val swDist = distance(sw.centerX, sw.centerY, b.x, b.y)
      val seDist = distance(se.centerX, se.centerY, b.x, b.y)

      val minDist = math.min(
        math.min(nwDist, neDist),
        math.min(swDist, seDist)
      )

      if (minDist == nwDist) {
        this.copy(nw = nw.insert(b))
      } else if (minDist == neDist) {
        this.copy(ne = ne.insert(b))
      } else if (minDist == swDist) {
        this.copy(sw = sw.insert(b))
      } else if (minDist == seDist) {
        this.copy(se = se.insert(b))
      } else {
        throw new Exception(s"$minDist did not equal any of [$nwDist, $neDist, $swDist, $seDist]")
      }
    }
  }

  case class Leaf(centerX: Float, centerY: Float, size: Float, bodies: Seq[Body])
    extends Quad {

    private def _mass = if (bodies.isEmpty) 0f else bodies.map(_.mass).sum
    private def _massX = if (_mass == 0f) centerX else bodies.map(b => b.mass * b.x).sum / _mass
    private def _massY = if (_mass == 0f) centerY else bodies.map(b => b.mass * b.y).sum / _mass

    val (mass, massX, massY) = (_mass: Float, _massX: Float, _massY: Float)
    val total: Int = bodies.length

    /**
      * If the size of a Leaf is greater than a predefined constant minimumSize, inserting an additonal body into
      * that Leaf quadtree creates a Fork quadtree with empty children, and adds all the bodies into that Fork
      * (including the new body). Otherwise, inserting creates another Leaf with all the existing bodies and the new
      * one.
      */
    def insert(b: Body): Quad =
      if (size > minimumSize) {
        val sizeByTwo = size / 2
        val sizeByFour = size / 4
        val nw = Empty(centerX - sizeByFour, centerY - sizeByFour, sizeByTwo)
        val ne = Empty(centerX + sizeByFour, centerY - sizeByFour, sizeByTwo)
        val sw = Empty(centerX - sizeByFour, centerY + sizeByFour, sizeByTwo)
        val se = Empty(centerX + sizeByFour, centerY + sizeByFour, sizeByTwo)
        (b +: bodies).foldLeft(Fork(nw, ne, sw, se))(_ insert _)
      } else {
        this.copy(size = size + 1, bodies = b +: bodies)
      }

  }

  def minimumSize = 0.00001f

  def gee: Float = 100.0f

  def delta: Float = 0.01f

  def theta = 0.5f

  def eliminationThreshold = 0.5f

  def force(m1: Float, m2: Float, dist: Float): Float = gee * m1 * m2 / (dist * dist)

  def distance(x0: Float, y0: Float, x1: Float, y1: Float): Float = {
    math.sqrt((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0)).toFloat
  }

  class Body(val mass: Float, val x: Float, val y: Float, val xspeed: Float, val yspeed: Float) {

    def updated(quad: Quad): Body = {
      var netforcex = 0.0f
      var netforcey = 0.0f

      def addForce(thatMass: Float, thatMassX: Float, thatMassY: Float): Unit = {
        val dist = distance(thatMassX, thatMassY, x, y)
        /* If the distance is smaller than 1f, we enter the realm of close
         * body interactions. Since we do not model them in this simplistic
         * implementation, bodies at extreme proximities get a huge acceleration,
         * and are catapulted from each other's gravitational pull at extreme
         * velocities (something like this:
         * http://en.wikipedia.org/wiki/Interplanetary_spaceflight#Gravitational_slingshot).
         * To decrease the effect of this gravitational slingshot, as a very
         * simple approximation, we ignore gravity at extreme proximities.
         */
        if (dist > 1f) {
          val dforce = force(mass, thatMass, dist)
          val xn = (thatMassX - x) / dist
          val yn = (thatMassY - y) / dist
          val dforcex = dforce * xn
          val dforcey = dforce * yn
          netforcex += dforcex
          netforcey += dforcey
        }
      }

      def isFarEnough(quad: Quad): Boolean = quad.size / distance(quad.massX, quad.massY, x, y) < theta

      def traverse(quad: Quad): Unit = (quad: Quad) match {
        case Empty(_, _, _) =>
        // no force
        case Leaf(_, _, _, bodies) => bodies.foreach(b => addForce(b.mass, b.x, b.y))
        // add force contribution of each body by calling addForce
        case Fork(nw, ne, sw, se) =>
          List(nw, ne, sw, se).foreach { q =>
            if (isFarEnough(nw)) addForce(q.mass, q.massX, q.massY)
            else traverse(q)
          }
        // see if node is far enough from the body, (quad.size / dist < theta)
        // or recursion is needed
      }

      traverse(quad)

      val nx = x + xspeed * delta
      val ny = y + yspeed * delta
      val nxspeed = xspeed + netforcex / mass * delta
      val nyspeed = yspeed + netforcey / mass * delta

      new Body(mass, nx, ny, nxspeed, nyspeed)
    }

  }

  val SECTOR_PRECISION = 8

  class SectorMatrix(val boundaries: Boundaries, val sectorPrecision: Int) {

    type Position = (Float, Float)

    val sectorSize: Float = boundaries.size / sectorPrecision
    val matrix = new Array[ConcBuffer[Body]](sectorPrecision * sectorPrecision)
    for (i <- 0 until matrix.length) matrix(i) = new ConcBuffer

    def +=(b: Body): SectorMatrix = {

      def position(body: Body): Position = (body.x, body.y)

      def sanitize: Position => Position =
        sanitizeXUpper _ andThen sanitizeXLower andThen sanitizeYLower andThen sanitizeYUpper

      def sanitizeXUpper(position: Position): Position = position match {
        case (x, y) => if (x > boundaries.maxX) (boundaries.maxX, y) else (x, y)
      }

      def sanitizeXLower(position: Position): Position = position match {
        case (x, y) => if (x < boundaries.minX) (boundaries.minX, y) else (x, y)
      }

      def sanitizeYUpper(position: Position): Position = position match {
        case (x, y) => if (y > boundaries.maxY) (x, boundaries.maxY) else (x, y)
      }

      def sanitizeYLower(position: Position): Position = position match {
        case (x, y) => if (y < boundaries.minY) (x, boundaries.minY) else (x, y)
      }

      def normalize(position: Position): Position = position match {
        case (x, y) => (x - boundaries.minX, y - boundaries.minY)
      }

      def bySectorSize(position: Position): Position = position match {
        case (x, y) => (x / sectorSize, y / sectorSize)
      }

      def floor(position: Position): (Int, Int) = position match {
        case (x, y) => (math.floor(x).toInt, math.floor(y).toInt)
      }

      def decode(position: (Int, Int)): Int = position match {
        case (x, y) => x + sectorPrecision * y
      }

      def matrixIndex: Body => Int =
        position _ andThen sanitize andThen normalize andThen bySectorSize andThen floor andThen decode

      matrix(matrixIndex(b)) += b

      this
    }

    def apply(x: Int, y: Int) = matrix(y * sectorPrecision + x)

    def combine(that: SectorMatrix): SectorMatrix = {
      for (i <- matrix.indices) matrix(i).combine(that.matrix(i))
      this
    }

    def toQuad(parallelism: Int): Quad = {
      def BALANCING_FACTOR = 4
      def quad(x: Int, y: Int, span: Int, achievedParallelism: Int): Quad = {
        if (span == 1) {
          val sectorSize = boundaries.size / sectorPrecision
          val centerX = boundaries.minX + x * sectorSize + sectorSize / 2
          val centerY = boundaries.minY + y * sectorSize + sectorSize / 2
          var emptyQuad: Quad = Empty(centerX, centerY, sectorSize)
          val sectorBodies = this(x, y)
          sectorBodies.foldLeft(emptyQuad)(_ insert _)
        } else {
          val nspan = span / 2
          val nAchievedParallelism = achievedParallelism * 4
          val (nw, ne, sw, se) =
            if (parallelism > 1 && achievedParallelism < parallelism * BALANCING_FACTOR) parallel(
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            ) else (
              quad(x, y, nspan, nAchievedParallelism),
              quad(x + nspan, y, nspan, nAchievedParallelism),
              quad(x, y + nspan, nspan, nAchievedParallelism),
              quad(x + nspan, y + nspan, nspan, nAchievedParallelism)
            )
          Fork(nw, ne, sw, se)
        }
      }

      quad(0, 0, sectorPrecision, 1)
    }

    override def toString = s"SectorMatrix(#bodies: ${matrix.map(_.size).sum})"
  }

  class TimeStatistics {
    private val timeMap = collection.mutable.Map[String, (Double, Int)]()

    def clear() = timeMap.clear()

    def timed[T](title: String)(body: =>T): T = {
      var res: T = null.asInstanceOf[T]
      val totalTime = /*measure*/ {
        val startTime = System.currentTimeMillis()
        res = body
        (System.currentTimeMillis() - startTime)
      }

      timeMap.get(title) match {
        case Some((total, num)) => timeMap(title) = (total + totalTime, num + 1)
        case None => timeMap(title) = (0.0, 0)
      }

      println(s"$title: ${totalTime} ms; avg: ${timeMap(title)._1 / timeMap(title)._2}")
      res
    }

    override def toString = {
      timeMap map {
        case (k, (total, num)) => k + ": " + (total / num * 100).toInt / 100.0 + " ms"
      } mkString("\n")
    }
  }
}