import scala.collection.mutable.{Map => MutableMap}

package object chapter3 {

  private def memoizeBuilder[A, B](table: MutableMap[A, B])(f: A => B)(x: A): B = {
    if (table.get(x).isEmpty) table += x -> f(x)
    table(x)
  }

  def memoize[A, B]: (A => B) => A => B = memoizeBuilder(MutableMap.empty[A, B])

}
