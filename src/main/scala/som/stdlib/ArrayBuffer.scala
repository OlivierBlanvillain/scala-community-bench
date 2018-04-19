/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package stdlib

/** An implementation of the `Buffer` class using an array to
 *  represent the assembled sequence internally. Append, update and random
 *  access take constant time (amortized time). Prepends and removes are
 *  linear in the buffer size.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#array_buffers "Scala's Collection Library overview"]]
 *  section on `Array Buffers` for more information.

 *
 *  @tparam A    the type of this arraybuffer's elements.
 *
 *  @define Coll `mutable.ArrayBuffer`
 *  @define coll array buffer
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `ArrayBuffer[B]` because an implicit of type `CanBuildFrom[ArrayBuffer, B, ArrayBuffer[B]]`
 *    is defined in object `ArrayBuffer`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `ArrayBuffer`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */

import scala.{Int, Array, Long, AnyRef, Any, Unit}
import java.lang.{String, Math}

class ArrayBuffer[A](protected val initialSize: Int) {
  import scala.collection.Traversable

  def this() = this(16)

  protected var array: Array[AnyRef] = new Array[AnyRef](Math.max(initialSize, 1))
  protected var size0: Int = 0

  //##########################################################################
  // implement/override methods of IndexedSeq[A]

  /** Returns the length of this resizable array.
   */
  def length: Int = size0
  def size = length

  def apply(idx: Int) = {
    if (idx >= size0) throw new scala.IndexOutOfBoundsException(idx.toString)
    array(idx).asInstanceOf[A]
  }

  def update(idx: Int, elem: A): Unit = {
    if (idx >= size0) throw new scala.IndexOutOfBoundsException(idx.toString)
    array(idx) = elem.asInstanceOf[AnyRef]
  }

  def foreach[U](f: A => U): Unit = {
    var i = 0
    // size is cached here because profiling reports a lot of time spent calling
    // it on every iteration.  I think it's likely a profiler ghost but it doesn't
    // hurt to lift it into a local.
    val top = size
    while (i < top) {
      f(array(i).asInstanceOf[A])
      i += 1
    }
  }

  /** Fills the given array `xs` with at most `len` elements of this
   *  traversable starting at position `start`.
   *
   *  Copying will stop once either the end of the current traversable is
   *  reached or `len` elements have been copied or the end of the array
   *  is reached.
   *
   *  @param  xs the array to fill.
   *  @param  start starting index.
   *  @param  len number of elements to copy
   */
   def copyToArray[B >: A](xs: Array[B], start: Int, len: Int): Unit = {
     val len1 = Math.min(Math.min(len, (xs.length - start)), length)
     if (len1 > 0) Array.copy(array, 0, xs, start, len1)
   }

  //##########################################################################

  /** Remove elements of this array at indices after `sz`.
   */
  def reduceToSize(sz: Int): Unit = {
    if (sz > size0) throw new scala.IllegalArgumentException()
    while (size0 > sz) {
      size0 -= 1
      array(size0) = null
    }
  }

  /** Ensure that the internal array has at least `n` cells. */
  protected def ensureSize(n: Int): Unit = {
    // Use a Long to prevent overflows
    val arrayLength: Long = array.length
    if (n > arrayLength) {
      var newSize: Long = arrayLength * 2
      while (n > newSize)
        newSize = newSize * 2
      // Clamp newSize to Int.MaxValue
      if (newSize > Int.MaxValue) newSize = Int.MaxValue

      val newArray: Array[AnyRef] = new Array(newSize.toInt)
      java.lang.System.arraycopy(array, 0, newArray, 0, size0)
      array = newArray
    }
  }

  /** Swap two elements of this array.
   */
  protected def swap(a: Int, b: Int): Unit = {
    val h = array(a)
    array(a) = array(b)
    array(b) = h
  }

  /** Move parts of the array.
   */
  protected def copy(m: Int, n: Int, len: Int): Unit = {
    scala.compat.Platform.arraycopy(array, m, array, n, len)
  }

  def clear(): Unit = { reduceToSize(0) }

  def sizeHint(len: Int): Unit = {
    if (len > size && len >= 1) {
      val newarray = new Array[AnyRef](len)
      java.lang.System.arraycopy(array, 0, newarray, 0, size0)
      array = newarray
    }
  }

  /** Appends a single element to this buffer and returns
   *  the identity of the buffer. It takes constant amortized time.
   *
   *  @param elem  the element to append.
   *  @return      the updated buffer.
   */
  def +=(elem: A): this.type = {
    ensureSize(size0 + 1)
    array(size0) = elem.asInstanceOf[AnyRef]
    size0 += 1
    this
  }

  /** Prepends a single element to this buffer and returns
   *  the identity of the buffer. It takes time linear in
   *  the buffer size.
   *
   *  @param elem  the element to prepend.
   *  @return      the updated buffer.
   */
  def +=:(elem: A): this.type = {
    ensureSize(size0 + 1)
    copy(0, 1, size0)
    array(0) = elem.asInstanceOf[AnyRef]
    size0 += 1
    this
  }

  /** Inserts new elements at the index `n`. Opposed to method
   *  `update`, this method will not replace an element with a new
   *  one. Instead, it will insert a new element at index `n`.
   *
   *  @param n     the index where a new element will be inserted.
   *  @param seq   the traversable object providing all elements to insert.
   *  @throws scala.IndexOutOfBoundsException if `n` is out of bounds.
   */
  def insertAll(n: Int, seq: Traversable[A]): Unit = {
    if (n < 0 || n > size0) throw new scala.IndexOutOfBoundsException(n.toString)
    val len = seq.size
    val newSize = size0 + len
    ensureSize(newSize)

    copy(n, n + len, size0 - n)
    seq.copyToArray(array.asInstanceOf[Array[Any]], n)
    size0 = newSize
  }

  /** Removes the element on a given index position. It takes time linear in
   *  the buffer size.
   *
   *  @param n       the index which refers to the first element to remove.
   *  @param count   the number of elements to remove.
   *  @throws   scala.IndexOutOfBoundsException if the index `n` is not in the valid range
   *            `0 <= n <= length - count` (with `count > 0`).
   *  @throws   scala.IllegalArgumentException if `count < 0`.
   */
  def remove(n: Int, count: Int): Unit = {
    if (count < 0) throw new scala.IllegalArgumentException("removing negative number of elements: " + count.toString)
    else if (count == 0) return  // Did nothing
    if (n < 0 || n > size0 - count) throw new scala.IndexOutOfBoundsException("at " + n.toString + " deleting " + count.toString)
    copy(n + count, n, size0 - (n + count))
    reduceToSize(size0 - count)
  }

  /** Removes the element at a given index position.
   *
   *  @param n  the index which refers to the element to delete.
   *  @return   the element that was formerly at position `n`.
   */
  def remove(n: Int): A = {
    val result = apply(n)
    remove(n, 1)
    result
  }

  def result: ArrayBuffer[A] = this

  /** Defines the prefix of the string representation.
   */
  def stringPrefix: String = "ArrayBuffer"

}
