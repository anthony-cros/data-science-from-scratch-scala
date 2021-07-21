package scratchscala.utils

// ===========================================================================
object ExtensionMethods { // note that many of those can be found in my utilities library: https://github.com/aptusproject/aptus-core

  implicit class Anything__[A](value: A) {
    def prt = { println(value); value } // for quick and dirty debug
    
    // just to avoid having to import scala.util.chaining._ everywhere (introduced in scala 2.13)
    @inline def pipe[B](f: A => B): B = f(value)
    @inline def tap [U](f: A => U): A = { f(value); value }
  }
  
  // ---------------------------------------------------------------------------
  implicit class Product__(product: Product) { // note: all case classes are Products
  	private lazy val _data: Map[String /* field name */, Any /* field value */] = MiscUtils.productData(product)

    def fieldValue(name: String): Any = _data(name) // scala isn't as "dynamic" as python by default 
  }
  
  // ---------------------------------------------------------------------------
  implicit class List__[A](coll: List[A]) {
    def listToPair: (A, A) = { require(coll.size == 2, coll.size); (coll(0), coll(1)) } // aka listToTuple2
    
    // dearly missed in stdlib, along with countBys and ListMap's counterparts (that maintain insertion order)
    def groupByKey[K, V](implicit ev: A <:< (K, V)): Map[K, List[V]] = coll.groupBy(_._1).mapValues(_.map(_._2))

    def zipSameSize[B](that: List[B])                       : List[(A, B)] = { require(coll.size == that.size, coll.size -> that.size);     coll.zip(that) }
    def zipSameSize[B](that: List[B], debug: List[_] => Any): List[(A, B)] = { require(coll.size == that.size, (debug(coll), debug(that))); coll.zip(that) }
  }

  // ---------------------------------------------------------------------------  
  implicit class Tuple2__[T](u: Tuple2[T, T]) {
	  def same      : Boolean = u._1 == u._2
    def pairToList: List[T] = List(u._1, u._2) // aka tupleToList
  }

}

// ===========================================================================
