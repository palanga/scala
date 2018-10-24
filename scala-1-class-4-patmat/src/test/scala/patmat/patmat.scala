import patmat.Huffman._

val a = List[Char]()

val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
val t3 = Fork(Leaf('t', 7), Leaf('x', 9), List('t', 'x'), 16)

val empty: List[Char] = List()
val abaac: List[Char] = List('a', 'b', 'a', 'a', 'c')
val unsorted: List[(Char, Int)] = List(('d', 2), ('a', 3), ('b', 7), ('c', 1))

times(a)
times(abaac)

until(singleton, combine)(List(t1, t2, t3)).head

createCodeTree(string2Chars("la acade hoy te vinimo a ver tenes que dar la vuelta"))


