/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._
import scala.collection.immutable.Queue

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot

  // optional
  var pendingQueue = Queue.empty[Operation]

  // optional
  def receive = normal

  // optional
  /** Accepts `Operation` and `GC` messages. */
  val normal: Receive = {
    case op: Operation =>
      root ! op
    case GC =>
      val newRoot = createRoot
      root ! CopyTo(newRoot)
      context.become(garbageCollecting(newRoot))
  }

  // optional
  /** Handles messages while garbage collection is performed.
    * `newRoot` is the root of the new binary tree where we want to copy
    * all non-removed elements into.
    */
  def garbageCollecting(newRoot: ActorRef): Receive = {
    case op: Operation  => pendingQueue = pendingQueue.enqueue(op)
    case GC             => // do nothing
    case CopyFinished   =>
      if (pendingQueue.isEmpty) {
        root = newRoot
        context.become(normal)
      } else {
        val (op, q) = pendingQueue.dequeue
        pendingQueue = q
        newRoot ! op
        self ! CopyFinished
      }
  }

}

object BinaryTreeNode {
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert(requester, id, x) =>
      if (elem == x) {
        removed = false
        requester ! OperationFinished(id)
      } else if (elem < x) {
        if (!subtrees.contains(Left))
          subtrees = subtrees + (Left -> context.actorOf(props(x, initiallyRemoved = false)))
        subtrees(Left) ! Insert(requester, id, x)
      } else {// elem > x
        if (!subtrees.contains(Right))
          subtrees = subtrees + (Right -> context.actorOf(props(x, initiallyRemoved = false)))
        subtrees(Right) ! Insert(requester, id, x)
      }
    case Contains(requester, id, x) =>
      if (elem == x) {
        requester ! ContainsResult(id, true)
      } else if (elem < x) {
        if (!subtrees.contains(Left))
          requester ! ContainsResult(id, false)
        else
          subtrees(Left) ! Contains(requester, id, x)
      } else {// elem > x
        if (!subtrees.contains(Right))
          requester ! ContainsResult(id, false)
        else
          subtrees(Right) ! Contains(requester, id, x)
      }
    case Remove(requester, id, x) =>
      if (elem == x) {
        removed = true
        requester ! OperationFinished(id)
      } else if (elem < x) {
        if (!subtrees.contains(Left))
          requester ! OperationFinished(id)
        else
          subtrees(Left) ! Remove(requester, id, x)
      } else {// elem > x
        if (!subtrees.contains(Right))
          requester ! OperationFinished(id)
        else
          subtrees(Right) ! Remove(requester, id, x)
      }
    case CopyTo(newRoot) =>
      val sub = subtrees.values
      context.become(copying(sub.toSet, false))
      if (!removed)
        newRoot ! Insert(self, -1, elem)
      sub foreach { _ ! CopyTo(newRoot) }
  }

  // optional
  /** `expected` is the set of ActorRefs whose replies we are waiting for,
    * `insertConfirmed` tracks whether the copy of this node to the new tree has been confirmed.
    */
  def copying(expected: Set[ActorRef], insertConfirmed: Boolean): Receive = {
    case OperationFinished =>
      if (expected.isEmpty) {
        context.parent ! CopyFinished
        self ! PoisonPill
      } else {
        context.become(copying(expected, true))
      }
    case CopyFinished =>
      val ns = expected - sender
      if (ns.isEmpty && insertConfirmed) {
        context.parent ! CopyFinished
        self ! PoisonPill
      } else {
        context.become(copying(ns, insertConfirmed))
      }
  }

}
