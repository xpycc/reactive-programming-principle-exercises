package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor, Stash, Cancellable }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import scala.concurrent._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  case object RetryPersist
  case object TimeOut

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor with Stash {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher
  import scala.language.postfixOps

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  def receive = {
    case JoinedPrimary => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  override val supervisorStrategy = SupervisorStrategy.stoppingStrategy

  var persistence: ActorRef = context.watch(context.actorOf(persistenceProps))
  arbiter ! Join

  var expectedSeq = 0L

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Insert(key, value, id) =>
      kv += key -> value
      val wPersist = Persist(key, Some(value), id)
      persistence ! wPersist
      replicators foreach { _ ! Replicate(key, Some(value), id) }
      context.become(leaderWaitingModifying(sender, wPersist), discardOld = false)
    case Remove(key, id) =>
      kv -= key
      val wPersist = Persist(key, None, id)
      persistence ! wPersist
      replicators foreach { _ ! Replicate(key, None, id) }
      context.become(leaderWaitingModifying(sender, wPersist), discardOld = false)
    case Get(key, id) =>
      sender ! GetResult(key, kv.get(key), id)
    case Terminated(_) =>
      persistence = context.watch(context.actorOf(persistenceProps))
    case Replicas(rep) =>
      val diff = replicatorDifference(rep)    // replicators --= replicatorDifference(rep)
      replicators --= diff                    //     is wrong because replicators' address changed after
                                              //     replicatorDifference is called
  }

  def replicatorDifference(rep: Set[ActorRef]): Set[ActorRef] = {
    var newMap = Map.empty[ActorRef, ActorRef]
    rep foreach { r =>
      if (secondaries.contains(r)) {
        newMap += r -> secondaries(r)
        secondaries -= r
      } else if (r != self) {
        val newRep = context.actorOf(Replicator.props(r))
        replicators += newRep
        newMap += r -> newRep
        kv foreach { e =>
          newRep ! Replicate(e._1, Some(e._2), -1)
          // newRep.tell(Replicate(e._1, Some(e._2), -1), Actor.noSender)     // will arise a dead letter
        }
      }
    }
    val ret = secondaries.values.toSet
    ret foreach { _ ! PoisonPill}
    secondaries = newMap
    ret
  }

  def leaderWaitingModifying(wSender: ActorRef, wPersist: Persist): Receive = {
    var pcan = context.system.scheduler.schedule(100 millis, 100 millis, self, RetryPersist)
    var rcan = context.system.scheduler.scheduleOnce(1 second, self, TimeOut)
    var rset = replicators

    {
      case Persisted(key, id) if !pcan.isCancelled && id == wPersist.id =>
        pcan.cancel()
        if (rset.isEmpty) {
          rcan.cancel()
          wSender ! OperationAck(id)
          unstashAll()
          context.unbecome()
        }
      case Replicated(key, id) if !rcan.isCancelled && id == wPersist.id =>
        rset -= sender
        if (rset.isEmpty && pcan.isCancelled) {
          rcan.cancel()
          wSender ! OperationAck(id)
          unstashAll()
          context.unbecome()
        }
      case TimeOut =>
        pcan.cancel()
        wSender ! OperationFailed(wPersist.id)
        unstashAll()
        context.unbecome()
      case Terminated(_) =>
        persistence = context.watch(context.actorOf(persistenceProps))
        persistence ! wPersist
      case RetryPersist =>
        persistence ! wPersist
      case Get(key, id) =>
        sender ! GetResult(key, kv.get(key), id)
      case Replicas(rep) =>
        val diff = replicatorDifference(rep)
        rset --= diff
        replicators --= diff
        if (rset.isEmpty && pcan.isCancelled) {
          rcan.cancel()
          wSender ! OperationAck(wPersist.id)
          unstashAll()
          context.unbecome()
        }
      case msg =>
        stash()
    }
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get(key, id) =>
      sender ! GetResult(key, kv.get(key), id)
    case Snapshot(key, option, seq) =>
      if (seq == expectedSeq) {
        val wPersist = Persist(key, option, seq)
        persistence ! Persist(key, option, seq)
        wPersist.valueOption match {
          case Some(value) => kv += key -> value
          case None => kv -= key
        }
        context.become(replicaWaitingPersistence(sender, wPersist), discardOld = false)
      } else if (seq < expectedSeq) {
        sender ! SnapshotAck(key, seq)
      }
    case Terminated(_) =>
      persistence = context.watch(context.actorOf(persistenceProps))
  }

  def replicaWaitingPersistence(wSender: ActorRef, wPersist: Persist): Receive = {
    var pcan = context.system.scheduler.schedule(100 millis, 100 millis, self, RetryPersist)

    {
      case Persisted(key, seq) if seq == expectedSeq =>
        pcan.cancel()
        wSender ! SnapshotAck(key, seq)
        expectedSeq += 1
        unstashAll()
        context.unbecome()
      case Get(key, id) =>
        sender ! GetResult(key, kv.get(key), id)
      case Terminated(_) =>
        persistence = context.watch(context.actorOf(persistenceProps))
        persistence ! wPersist
      case RetryPersist =>
        persistence ! wPersist
      case msg =>
        stash()
    }
  }

}
