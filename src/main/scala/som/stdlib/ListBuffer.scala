/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package stdlib

import java.lang.String
import scala.{Int, Any, Boolean, AnyRef, Unit}

/** A `Buffer` implementation backed by a list. It provides constant time
 *  prepend and append. Most other operations are linear.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#list_buffers "Scala's Collection Library overview"]]
 *  section on `List Buffers` for more information.
 *
 *  @tparam A    the type of this list buffer's elements.
 *
 *  @define Coll `ListBuffer`
 *  @define coll list buffer
 *  @define thatinfo the class of the returned collection. In the standard library configuration,
 *    `That` is always `ListBuffer[B]` because an implicit of type `CanBuildFrom[ListBuffer, B, ListBuffer[B]]`
 *    is defined in object `ListBuffer`.
 *  @define bfinfo an implicit value of class `CanBuildFrom` which determines the
 *    result class `That` from the current representation type `Repr`
 *    and the new element type `B`. This is usually the `canBuildFrom` value
 *    defined in object `ListBuffer`.
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
final class ListBuffer[A] {
  def foreach(f: A => Unit): Unit = toList.foreach(f)
  /** Expected invariants:
   *  If start.isEmpty, last0 == null
   *  If start.nonEmpty, last0 != null
   *  If len == 0, start.isEmpty
   *  If len > 0, start.nonEmpty
   */
  private var start: List[A] = Nil
  private var last0: ::[A] = _
  private var exported: Boolean = false
  private var len = 0

  protected def underlying: List[A] = start

  /** The current length of the buffer.
   *
   *  This operation takes constant time.
   */
  def length = len

  // Don't use the inherited size, which forwards to a List and is O(n).
  def size = length

  // Override with efficient implementations using the extra size information available to ListBuffer.
  def isEmpty: Boolean = len == 0
  def nonEmpty: Boolean = len > 0

  /** Replaces element at index `n` with the new element
   *  `newelem`. Takes time linear in the buffer size. (except the
   *  first element, which is updated in constant time).
   *
   *  @param n  the index of the element to replace.
   *  @param x  the new element.
   *  @throws IndexOutOfBoundsException if `n` is out of bounds.
   */
  def update(n: Int, x: A): Unit = {
    // We check the bounds early, so that we don't trigger copying.
    if (n < 0 || n >= len) throw new scala.IndexOutOfBoundsException(n.toString)
    if (exported) copy()
    if (n == 0) {
      val newElem = new :: (x, start.tail)
      if (last0 eq start) {
        last0 = newElem
      }
      start = newElem
    } else {
      var cursor = start
      var i = 1
      while (i < n) {
        cursor = cursor.tail
        i += 1
      }
      val newElem = new :: (x, cursor.tail.tail)
      if (last0 eq cursor.tail) {
        last0 = newElem
      }
      cursor.asInstanceOf[::[A]].next = newElem
    }
  }

  /** Appends a single element to this buffer. This operation takes constant time.
   *
   *  @param x  the element to append.
   *  @return   this $coll.
   */
  def += (x: A): this.type = {
    if (exported) copy()
    if (isEmpty) {
      last0 = new :: (x, Nil)
      start = last0
    } else {
      val last1 = last0
      last0 = new :: (x, Nil)
      last1.next = last0
    }
    len += 1
    this
  }

  /** Clears the buffer contents.
   */
  def clear(): Unit = {
    start = Nil
    last0 = null
    exported = false
    len = 0
  }

  /** Prepends a single element to this buffer. This operation takes constant
   *  time.
   *
   *  @param x  the element to prepend.
   *  @return   this $coll.
   */
  def +=: (x: A): this.type = {
    if (exported) copy()
    val newElem = new :: (x, start)
    if (isEmpty) last0 = newElem
    start = newElem
    len += 1
    this
  }

  /** Reduce the length of the buffer, and null out last0
   *  if this reduces the length to 0.
   */
  private def reduceLengthBy(num: Int): Unit = {
    len -= num
    if (len <= 0)   // obviously shouldn't be < 0, but still better not to leak
      last0 = null
  }

  /** Removes a given number of elements on a given index position. May take
   *  time linear in the buffer size.
   *
   *  @param n         the index which refers to the first element to remove.
   *  @param count     the number of elements to remove.
   *  @throws   IndexOutOfBoundsException if the index `n` is not in the valid range
   *            `0 <= n <= length - count` (with `count > 0`).
   *  @throws   IllegalArgumentException if `count < 0`.
   */
  def remove(n: Int, count: Int): Unit = {
    if (count < 0) throw new scala.IllegalArgumentException("removing negative number of elements: " + count.toString)
    else if (count == 0) return  // Nothing to do
    if (n < 0 || n > len - count) throw new scala.IndexOutOfBoundsException("at " + n.toString + " deleting " + count.toString)
    if (exported) copy()
    val n1 = java.lang.Math.max(n, 0)
    val count1 = java.lang.Math.min(count, (len - n1))
    if (n1 == 0) {
      var c = count1
      while (c > 0) {
        start = start.tail
        c -= 1
      }
    } else {
      var cursor = start
      var i = 1
      while (i < n1) {
        cursor = cursor.tail
        i += 1
      }
      var c = count1
      while (c > 0) {
        if (last0 eq cursor.tail) last0 = cursor.asInstanceOf[::[A]]
        cursor.asInstanceOf[::[A]].next = cursor.tail.tail
        c -= 1
      }
    }
    reduceLengthBy(count1)
  }

// Implementation of abstract method in Builder

  /** Returns the accumulated `List`.
   *
   *  This method may be called multiple times to obtain snapshots of the list in different stages of construction.
   */
  def result: List[A] = toList

  /** Converts this buffer to a list. Takes constant time. The buffer is
   *  copied lazily, the first time it is mutated.
   */
  def toList: List[A] = {
    exported = !isEmpty
    start
  }

// New methods in ListBuffer

  /** Prepends the elements of this buffer to a given list
   *
   *  @param xs   the list to which elements are prepended
   */
  def prependToList(xs: List[A]): List[A] = {
    if (isEmpty) xs
    else {
      if (exported) copy()
      last0.next = xs
      toList
    }
  }

// Overrides of methods in Buffer

  /** Removes the element on a given index position. May take time linear in
   *  the buffer size.
   *
   *  @param  n  the index which refers to the element to delete.
   *  @return n  the element that was formerly at position `n`.
   *  @note      an element must exists at position `n`.
   *  @throws IndexOutOfBoundsException if `n` is out of bounds.
   */
  def remove(n: Int): A = {
    if (n < 0 || n >= len) throw new scala.IndexOutOfBoundsException(n.toString())
    if (exported) copy()
    var old = start.head
    if (n == 0) {
      start = start.tail
    } else {
      var cursor = start
      var i = 1
      while (i < n) {
        cursor = cursor.tail
        i += 1
      }
      old = cursor.tail.head
      if (last0 eq cursor.tail) last0 = cursor.asInstanceOf[::[A]]
      cursor.asInstanceOf[::[A]].next = cursor.tail.tail
    }
    reduceLengthBy(1)
    old
  }

  /** Remove a single element from this buffer. May take time linear in the
   *  buffer size.
   *
   *  @param elem  the element to remove.
   *  @return      this $coll.
   */
  def -= (elem: A): this.type = {
    if (exported) copy()
    if (isEmpty) {}
    else if (start.head == elem) {
      start = start.tail
      reduceLengthBy(1)
    }
    else {
      var cursor = start
      while (!cursor.tail.isEmpty && cursor.tail.head != elem) {
        cursor = cursor.tail
      }
      if (!cursor.tail.isEmpty) {
        val z = cursor.asInstanceOf[::[A]]
        if (z.next == last0)
          last0 = z
        z.next = cursor.tail.tail
        reduceLengthBy(1)
      }
    }
    this
  }

  /** Selects the last element.
   *
   *  Runs in constant time.
   *
   *  @return the last element of this buffer.
   *  @throws NoSuchElementException if this buffer is empty.
   */
  def last: A =
    if (last0 eq null) throw new scala.NoSuchElementException("last of empty ListBuffer")
    else last0.head

  /** Optionally selects the last element.
   *
   *  Runs in constant time.
   *
   *  @return `Some` of the last element of this buffer if the buffer is nonempty, `None` if it is empty.
   */
  def lastOption: Option[A] = if (last0 eq null) None else Some(last0.head)

  // Private methods

  /** Copy contents of this buffer */
  private def copy(): Unit = {
    if (isEmpty) return
    var cursor = start
    val limit = last0.tail
    clear()
    while (cursor ne limit) {
      this += cursor.head
      cursor = cursor.tail
    }
  }

  override def equals(that: Any): Boolean = that match {
    case that: ListBuffer[_] => this.start equals that.start
    case _                   => super.equals(that)
  }

  /** Defines the prefix of the string representation.
   *
   *  @return the string representation of this buffer.
   */
  def stringPrefix: String = "ListBuffer"
}

