package scala.collection

package object next {
  implicit class NextIteratorExtensions[A](private val it: Iterator[A]) extends AnyVal {
    def dropRight(i: Int): Iterator[A] = 
      if (i <= 0) it 
      else new AbstractIterator[A]{
        private[this] var buffer: Array[AnyRef] = null
        private[this] var current = -1
        private[this] var fetched = false

        private[this] def moveIndex(c: Int, n: Int) = (c + n) % i
        private[this] def init(): Boolean = {
          buffer = new Array(i)
          var x = 0
          while (x < i-1 && it.hasNext) {
            buffer(x) = it.next().asInstanceOf[AnyRef]
            x += 1
          }
          x == i-1 && it.hasNext
        }
        
        def hasNext: Boolean = fetched || ((buffer != null || init()) && {
          val e = it.next()
          current = moveIndex(current, 1)
          val last = moveIndex(current, i-1)
          buffer(last) = e.asInstanceOf[AnyRef]
          fetched = true
          it.hasNext
        })
        def next(): A = 
          if (hasNext) {
            fetched = false
            buffer(current).asInstanceOf[A]
          }
          else Iterator.empty.next()
      }
  }
}
