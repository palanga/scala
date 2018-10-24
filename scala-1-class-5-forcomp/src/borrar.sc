val xs = 0 until 10
val ys = 0 until 10

for {
  x <- xs
  y <- ys
  if y == x * x
} yield (x, y)

def isSafe(col: Int, queens: List[Int]): Boolean = {
  queens.forall(q => Math.abs(q - col) > 1)
}

val lala = Map('2' -> "abc", '3' -> "def")

val lalala = for {
  (digit, letters) <- lala
  letter <- letters
} yield letter -> digit;
