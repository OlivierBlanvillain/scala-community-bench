/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2013, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */


package stdlib

import scala.{Nothing, Int, Unit, Boolean}
/** Factory object for the `mutable.Stack` class.
 *
 *  $factoryInfo
 *  @define coll mutable stack
 *  @define Coll `mutable.Stack`
 */
object Stack {
  val empty: Stack[Nothing] = new Stack(Nil)
}

/** A stack implements a data structure which allows to store and retrieve
 *  objects in a last-in-first-out (LIFO) fashion.
 *
 *  @tparam A    type of the elements contained in this stack.
 *
 *  @author  Matthias Zenger
 *  @author  Martin Odersky
 *  @version 2.8
 *  @since   1
 *  @see [[http://docs.scala-lang.org/overviews/collections/concrete-mutable-collection-classes.html#stacks "Scala's Collection Library overview"]]
 *  section on `Stacks` for more information.
 *  @define Coll `Stack`
 *  @define coll stack
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 */
// @deprecated("Stack is an inelegant and potentially poorly-performing wrapper around List. Use a List assigned to a var instead.", "2.12.0")
class Stack[A] private (var elems: List[A]) {
  def this() = this(Nil)

  def companion = Stack

  /** Checks if the stack is empty.
   *
   *  @return true, iff there is no element on the stack
   */
  def isEmpty: Boolean = elems.isEmpty

  /** The number of elements in the stack */
  def length = elems.length

  /** Push an element on the stack.
   *
   *  @param   elem       the element to push on the stack.
   *  @return the stack with the new element on top.
   */
  def push(elem: A): this.type = { elems = elem :: elems; this }

  /** Returns the top element of the stack. This method will not remove
   *  the element from the stack. An error is signaled if there is no
   *  element on the stack.
   *
   *  @throws java.util.NoSuchElementException
   *  @return the top element
   */
  def top: A =
    elems.head

  /** Removes the top element from the stack.
   *
   *  @throws java.util.NoSuchElementException
   *  @return the top element
   */
  def pop(): A = {
    val res = elems.head
    elems = elems.tail
    res
  }

  /**
   * Removes all elements from the stack. After this operation completed,
   * the stack will be empty.
   */
  def clear(): Unit = elems = Nil

  /** This method clones the stack.
   *
   *  @return  a stack with the same elements.
   */
  override def clone(): Stack[A] = new Stack[A](elems)
}
