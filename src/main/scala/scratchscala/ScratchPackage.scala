// ===========================================================================
package object scratchscala extends _package

// --------------------------------------------------------------------------- 
package scratchscala {

  trait _package { // as trait so can be reused

    // save some imports
    val random          = utils.PythonRndm
    val Counter         = utils.Counter
  
    type ArrayBuffer[T] = collection.mutable.ArrayBuffer[T]
    val  ArrayBuffer    = collection.mutable.ArrayBuffer
  
    type MutableList[T] = collection.mutable.ArrayDeque[T]
    val  MutableList    = collection.mutable.ArrayDeque    
      
    val Utils = utils.MiscUtils

    // ---------------------------------------------------------------------------
    // clearer semantics
    type Index = Int  
    type Count = Int  
    type Size  = Int
   
    // ===========================================================================
    import me.shadaj.scalapy.py
    import me.shadaj.scalapy.py.SeqConverters
    import me.shadaj.scalapy.readwrite.Writer

    lazy val plt = py.module("matplotlib.pyplot")

    implicit def _toPythonProxy[T : Writer](values: Seq[T]): py.Any = values.toPythonProxy // for smoother integration with python

    // ===========================================================================
    // extension methods for convenience, e.g "hello".prt instead println("hello")
    import scratchscala.utils.ExtensionMethods._

      //TODO: t210721090902 - is there a cleaner way to "alias" extension methods in scala?
      @inline implicit def toAnything__  [A](x:      A )      = new Anything__[A](x)
      @inline implicit def toProduct__      (x: Product)      = new Product__    (x)
      @inline implicit def toList__      [A](x: List[A])      = new List__    [A](x)
      @inline implicit def toTuple2__    [A](x: Tuple2[A, A]) = new Tuple2__  [A](x)
  }

}

// ===========================================================================
