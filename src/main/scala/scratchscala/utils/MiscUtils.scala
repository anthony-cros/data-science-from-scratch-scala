package scratchscala.utils

// ===========================================================================
object MiscUtils { // note that many of those can be found in my utilities library: https://github.com/aptusproject/aptus-core

  def readTable(path: String, sep: Char): List[List[String]] = { // not a very diligent parser
    val src = io.Source.fromFile(path)
    val lines = src.getLines().toList
    src.close()
    lines.map(_.split(sep).toList)
  }
  
  // ---------------------------------------------------------------------------
  def log2(x: Double) = math.log10(x) / math.log10(2.0) // annoyingly missing
  
  // ---------------------------------------------------------------------------
  def zip[A,  B, C, D](a: Iterable[A], b: Iterable[B], c: Iterable[C],  d: Iterable[D]): List[(A, B, C, D)] = 
    a.zip(b).zip(c).zip(d)
      .map { case (((a, b), c), d) => (a, b, c, d) }
      .toList

  // ---------------------------------------------------------------------------
  def compareIterables[T : Ordering](x: Iterable[T], y: Iterable[T]): Boolean = { // note: assumes same size here
    require(x.size == y.size, x.size -> y.size)
    val ord = implicitly[Ordering[T]]
    (x.zip(y).view.map { case (a, b) => ord.compare(a, b) }.find(_ != 0).getOrElse(0)) < 0
  }	

  // ---------------------------------------------------------------------------
  def productData(product: Product): Map[/* field name */ String, /* field value */ Any] =
    product
         // note: this will be present in 2.13 as "productElementNames"
          .getClass
          .getDeclaredFields
          .toList
          .map(_.getName)
        .zip(product.productIterator.toList)
        .toMap
} 

// ===========================================================================
