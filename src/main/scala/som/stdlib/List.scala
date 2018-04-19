package stdlib

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.tailrec

/** A class for immutable linked lists representing ordered collections
  *  of elements of type `A`.
  *
  *  This class comes with two implementing case classes `scala.Nil`
  *  and `scala.::` that implement the abstract members `isEmpty`,
  *  `head` and `tail`.
  *
  *  This class is optimal for last-in-first-out (LIFO), stack-like access patterns. If you need another access
  *  pattern, for example, random access or FIFO, consider using a collection more suited to this than `List`.
  *
  *  $usesMutableState
  *
  *  ==Performance==
  *  '''Time:''' `List` has `O(1)` prepend and head/tail access. Most other operations are `O(n)` on the number of elements in the list.
  *  This includes the index-based lookup of elements, `length`, `append` and `reverse`.
  *
  *  '''Space:''' `List` implements '''structural sharing''' of the tail list. This means that many operations are either
  *  zero- or constant-memory cost.
  *  {{{
  *  val mainList = List(3, 2, 1)
  *  val with4 =    4 :: mainList  // re-uses mainList, costs one :: instance
  *  val with42 =   42 :: mainList // also re-uses mainList, cost one :: instance
  *  val shorter =  mainList.tail  // costs nothing as it uses the same 2::1::Nil instances as mainList
  *  }}}
  *
  *  @example {{{
  *  // Make a list via the companion object factory
  *  val days = List("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  *
  *  // Make a list element-by-element
  *  val when = "AM" :: "PM" :: Nil
  *
  *  // Pattern match
  *  days match {
  *    case firstDay :: otherDays =>
  *      println("The first day of the week is: " + firstDay)
  *    case Nil =>
  *      println("There don't seem to be any week days.")
  *  }
  *  }}}
  *
  *  @note The functional list is characterized by persistence and structural sharing, thus offering considerable
  *        performance and space consumption benefits in some scenarios if used correctly.
  *        However, note that objects having multiple references into the same functional list (that is,
  *        objects that rely on structural sharing), will be serialized and deserialized with multiple lists, one for
  *        each reference to it. I.e. structural sharing is lost after serialization/deserialization.
  *
  *  @author  Martin Odersky and others
  *  @version 2.8
  *  @since   1.0
  *  @see  [[http://docs.scala-lang.org/overviews/collections/concrete-immutable-collection-classes.html#lists "Scala's Collection Library overview"]]
  *  section on `Lists` for more information.
  *
  *  @define coll list
  *  @define Coll `List`
  *  @define orderDependent
  *  @define orderDependentFold
  *  @define mayNotTerminateInf
  *  @define willNotTerminateInf
  */

import scala.{AnyRef, `inline`, Nothing, Any, Boolean, Int, Unit, PartialFunction}

sealed abstract class List[+A] {
  def head: A
  def tail: List[A]
  def isEmpty: Boolean
  def nonEmpty = !isEmpty

  /** Adds an element at the beginning of this list.
    *  @param elem the element to prepend.
    *  @return  a list which contains `x` as first element and
    *           which continues with this list.
    *
    *  @usecase def ::(elem: A): List[A]
    *    @inheritdoc
    *
    *    Example:
    *    {{{1 :: List(2, 3) = List(2, 3).::(1) = List(1, 2, 3)}}}
    */
  def :: [B >: A](elem: B): List[B] =  new ::(elem, this)

  /** Adds the elements of a given list in reverse order in front of this list.
    *  `xs reverse_::: ys` is equivalent to
    *  `xs.reverse ::: ys` but is more efficient.
    *
    *  @param prefix the prefix to reverse and then prepend
    *  @return       the concatenation of the reversed prefix and the current list.
    */
  def reverse_:::[B >: A](prefix: List[B]): List[B] = {
    var these: List[B] = this
    var pres = prefix
    while (!pres.isEmpty) {
      these = pres.head :: these
      pres = pres.tail
    }
    these
  }

  def prepended[B >: A](elem: B): List[B] = elem :: this

  def take(n: Int): List[A] = if (isEmpty || n <= 0) Nil else {
    val h = new ::(head, Nil)
    var t = h
    var rest = tail
    var i = 1
    while ({if (rest.isEmpty) return this; i < n}) {
      i += 1
      val nx = new ::(rest.head, Nil)
      t.next = nx
      t = nx
      rest = rest.tail
    }
    h
  }

  // dropRight is inherited from LinearSeq

  final def map[B](f: A => B): List[B] = {
    if (this eq Nil) Nil else {
      val h = new ::[B](f(head), Nil)
      var t: ::[B] = h
      var rest = tail
      while (rest ne Nil) {
        val nx = new ::(f(rest.head), Nil)
        t.next = nx
        t = nx
        rest = rest.tail
      }
      h
    }
  }

  final def flatMap[B](f: A => List[B]): List[B] = {
    if (this eq Nil) Nil else {
      var rest = this
      var found = false
      var h: ::[B] = null
      var t: ::[B] = null
      while (rest ne Nil) {
        f(rest.head).foreach { b =>
          if (!found) {
            h = new ::(b, Nil)
            t = h
            found = true
          }
          else {
            val nx = new ::(b, Nil)
            t.next = nx
            t = nx
          }
        }
        rest = rest.tail
      }
      if (!found) Nil else h
    }
  }

  @inline final def dropWhile(p: A => Boolean): List[A] = {
    @tailrec
    def loop(xs: List[A]): List[A] =
      if (xs.isEmpty || !p(xs.head)) xs
      else loop(xs.tail)

    loop(this)
  }

  // Overridden with an implementation identical to the inherited one (at this time)
  // solely so it can be finalized and thus inlinable.
  @inline final def foreach[U](f: A => U): Unit = {
    var these = this
    while (!these.isEmpty) {
      f(these.head)
      these = these.tail
    }
  }

  final def reverse: List[A] = {
    var result: List[A] = Nil
    var these = this
    while (!these.isEmpty) {
      result = these.head :: result
      these = these.tail
    }
    result
  }

  final def foldRight[B](z: B)(op: (A, B) => B): B = {
    var acc = z
    var these: List[A] = reverse
    while (!these.isEmpty) {
      acc = op(these.head, acc)
      these = these.tail
    }
    acc
  }

  // Copy/Paste overrides to avoid interface calls inside loops.

  final def length: Int = {
    var these = this
    var len = 0
    while (!these.isEmpty) {
      len += 1
      these = these.tail
    }
    len
  }

  final def forall(p: A => Boolean): Boolean = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (!p(these.head)) return false
      these = these.tail
    }
    true
  }

  final def exists(p: A => Boolean): Boolean = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (p(these.head)) return true
      these = these.tail
    }
    false
  }

  final def contains[A1 >: A](elem: A1): Boolean = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (these.head == elem) return true
      these = these.tail
    }
    false
  }

  final def find(p: A => Boolean): Option[A] = {
    var these: List[A] = this
    while (!these.isEmpty) {
      if (p(these.head)) return Some(these.head)
      these = these.tail
    }
    None
  }

  def className = "List"

  /** Builds a new list by applying a function to all elements of this list.
    *  Like `xs map f`, but returns `xs` unchanged if function
    *  `f` maps all elements to themselves (as determined by `eq`).
    *
    *  @param f      the function to apply to each element.
    *  @tparam B     the element type of the returned collection.
    *  @return       a list resulting from applying the given function
    *                `f` to each element of this list and collecting the results.
    *
    *  @usecase def mapConserve(f: A => A): List[A]
    *    @inheritdoc
    */
  @`inline` final def mapConserve[B >: A <: AnyRef](f: A => B): List[B] = {
    // Note to developers: there exists a duplication between this function and `reflect.internal.util.Collections#map2Conserve`.
    // If any successful optimization attempts or other changes are made, please rehash them there too.
    //@tailrec
    def loop(mappedHead: List[B] = Nil, mappedLast: ::[B], unchanged: List[A], pending: List[A]): List[B] = {
      if (pending.isEmpty) {
        if (mappedHead eq null) unchanged
        else {
          mappedLast.next = (unchanged: List[B])
          mappedHead
        }
      }
      else {
        val head0 = pending.head
        val head1 = f(head0)

        if (head1 eq head0.asInstanceOf[AnyRef])
          loop(mappedHead, mappedLast, unchanged, pending.tail)
        else {
          var xc = unchanged
          var mappedHead1: List[B] = mappedHead
          var mappedLast1: ::[B] = mappedLast
          while (xc ne pending) {
            val next = new ::[B](xc.head, Nil)
            if (mappedHead1 eq null) mappedHead1 = next
            if (mappedLast1 ne null) mappedLast1.next = next
            mappedLast1 = next
            xc = xc.tail
          }
          val next = new ::(head1, Nil)
          if (mappedHead1 eq null) mappedHead1 = next
          if (mappedLast1 ne null) mappedLast1.next = next
          mappedLast1 = next
          val tail0 = pending.tail
          loop(mappedHead1, mappedLast1, tail0, tail0)

        }
      }
    }
    loop(null, null, this, this)
  }

  final def toList: List[A] = this

  // Override for performance
  override def equals(o: scala.Any): Boolean = {
    @tailrec def listEq(a: List[_], b: List[_]): Boolean =
      (a eq b) || {
        if (a.nonEmpty && b.nonEmpty && a.head == b.head) {
          listEq(a.tail, b.tail)
        }
        else {
          a.isEmpty && b.isEmpty
        }
      }

    o match {
      case that: List[_] => listEq(this, that)
      case _ => super.equals(o)
    }
  }

}

case class :: [+A](val head: A, var next: List[A @uncheckedVariance]) // sound because `next` is used only locally
  extends List[A] {
  def isEmpty: Boolean = false
  def headOption: Some[A] = Some(head)
  def tail: List[A] = next
}

case object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new scala.NoSuchElementException("head of empty list")
  def headOption: None.type = None
  def tail: Nothing = throw new scala.UnsupportedOperationException("tail of empty list")
  def last: Nothing = throw new scala.NoSuchElementException("last of empty list")
  def init: Nothing = throw new scala.UnsupportedOperationException("init of empty list")
  def knownSize: Int = 0
}

/**
  * $factoryInfo
  * @define coll list
  * @define Coll `List`
  */
object List {
  def empty[A]: List[A] = Nil
}
