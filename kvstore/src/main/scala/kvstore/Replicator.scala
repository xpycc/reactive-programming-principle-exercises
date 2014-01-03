package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)
  
  case object RetrySnapShot

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  import scala.language.postfixOps
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  // map from sequence number to pair of sender and request
  var acks = Map.empty[Long, (ActorRef, Replicate)]
  // a sequence of not-yet-sent snapshots (you can disregard this if not implementing batching)
  var pending = Vector.empty[Snapshot]
  
  context.system.scheduler.schedule(100 millis, 100 millis, self, RetrySnapShot)
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  /* TODO Behavior for the Replicator. */
  def receive: Receive = {
    case rep: Replicate =>
      acks += _seqCounter -> (sender, rep)
      replica ! Snapshot(rep.key, rep.valueOption, _seqCounter)
      nextSeq
    case SnapshotAck(key, seq) if acks.contains(seq) =>
      acks(seq) match {
        case (actor, Replicate(key, _, id)) =>
          actor ! Replicated(key, id)
      }
      acks -= seq
    case RetrySnapShot if !acks.isEmpty =>
      acks.minBy(_._1) match {
        case (seq, (_, replicate)) =>
          replica ! Snapshot(replicate.key, replicate.valueOption, seq)
      }
  }

}
