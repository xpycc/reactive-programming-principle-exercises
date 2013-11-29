package nodescala

import scala.language.postfixOps
import scala.util.{Try, Success, Failure}
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 seconds)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }
  
  test("FutureCompanionOps.all test") {
    def create(msec: Long, v: Int) = Future{ blocking { Thread.sleep(msec); v } }
    val xs = List( create(100, 1), create(200, 2), create(300, 3) )

    assert(Await.result(Future.all(xs), 30 seconds) === List(1, 2, 3))		// shuold not throw
    
    val ys = List( create(400, 4), create(300, 3), Future( blocking { Thread.sleep(200); throw new NoSuchElementException("my goal") } ),
        create(100, 1) )
    
    try {
      Await.result(Future.all(ys), 30 seconds)
      assert(false)
    } catch {
      case e: RuntimeException =>
        if (e.getMessage() != "One of fs failed." || e.getCause().getMessage() != "my goal")
          assert(false)
        // else ok
      // otherwise do not catch
    }
  }
  
  test("FutureCompanionOps.any test") {
    def create(msec: Long, v: Int) = Future{ blocking { Thread.sleep(msec); v } }
    val xs = List( create(100, 1), create(200, 2), create(300, 3) )

    assert(Await.result(Future.any(xs), 30 seconds) === 1)		// shuold not throw
    
    val ys = List( create(400, 4), create(300, 3), Future( blocking { Thread.sleep(100); throw new RuntimeException("answer 1") } ) )
    
    try {
      Await.result(Future.any(ys), 30 seconds)
      assert(false)
    } catch {
      case e: RuntimeException =>
        if (e.getMessage() != "answer 1") assert(false)
        // else ok
      // otherwise do not catch
    }
    
    val zs = List( create(400, 4), create(200, 2), Future( blocking { Thread.sleep(300); throw new RuntimeException("3") } ) )
    
    assert(Await.result(Future.any(zs), 30 second) === 2)
  }
  
  test("FutureCompanionOps.delay test") {
    Await.result(Future.delay(0.3 second), 10 second)
    try {
      Await.result(Future.delay(0.3 second), 0.1 second)
    } catch {
      case t: TimeoutException => // ok
      // otherwise do not catch
    }
    Await.result(Future.delay(0.1 second), 0.5 second)
    try {
      Await.result(Future.delay(0.1 second), 0.05 second)
    } catch {
      case t: TimeoutException => // ok
      // otherwise do not catch
    }
  }
  
  test("FutureOps.now test") {
    val f = Future( 1 )
    val g = Future( throw new RuntimeException("Synthetic") )
    val h = Future( blocking { Thread.sleep(100); 1 } )
    
    assert(f.now === 1)
    
    try {
      g.now
    } catch {
      case e: RuntimeException =>
        if (e.getMessage() != "Synthetic")
          assert(false)
        // else ok
      // otherwise do not catch
    }
    
    try {
      h.now
    } catch {
      case e: NoSuchElementException =>
        if (e.getMessage() != "Not ready")
          assert(false)
        // else ok
      // otherwise do not catch
    }
  }
  
  test("FutureOps.continueWith test") {
    def contCareless(a: Future[Int]): String = (Await.result(a, 1 second) + 1).toString
    val f = Future( 1 )
    assert(Await.result(f.continueWith(contCareless), 1 second) === "2")

    def contThrow(a: Future[Int]): String = throw new RuntimeException("Synthetic")
    try Await.result(f.continueWith(contThrow), 1 second) catch {
      case e: RuntimeException =>
        if (e.getMessage() != "Synthetic")
          assert(false)
        // else ok
        // otherwise do not catch
    }
    
    val g: Future[Int] = Future( throw new RuntimeException("g failed") )
    try Await.result(g.continueWith(contCareless), 1 second) catch {
      case e: RuntimeException =>
        if (e.getMessage() != "g failed")
          assert(false)
        // else ok
      // otherwise do not catch
    }
    
    def contThrow2(a: Future[Int]): String = try {
      (Await.result(a, 1 second) + 1).toString
    } catch {
      case _:Exception => throw new RuntimeException("Supressed")
    }
    try Await.result(g.continueWith(contThrow2), 1 second) catch {
      case e: RuntimeException =>
        if (e.getMessage() != "Supressed")
          assert(false)
        // else ok
      // otherwise do not catch
    }
    assert(Await.result(f.continueWith(contThrow2), 1 second) === "2")
  }
  
  test("FutureOps.continue test") {
    def contCareless(a: Try[Int]): String = (a.get + 1).toString
    val f = Future( 1 )
    assert(Await.result(f.continue(contCareless), 1 second) === "2")

    def contThrow(a: Try[Int]): String = throw new RuntimeException("Synthetic")
    try Await.result(f.continue(contThrow), 1 second) catch {
      case e: RuntimeException =>
        if (e.getMessage() != "Synthetic")
          assert(false)
        // else ok
        // otherwise do not catch
    }
    
    val g: Future[Int] = Future( throw new RuntimeException("g failed") )
    try Await.result(g.continue(contCareless), 1 second) catch {
      case e: RuntimeException =>
        if (e.getMessage() != "g failed")
          assert(false)
        // else ok
      // otherwise do not catch
    }
    
    def contThrow2(a: Try[Int]): String = a match {
      case Success(x) => (x + 1).toString
      case Failure(_) => throw new RuntimeException("Supressed")
    } 
    try Await.result(g.continue(contThrow2), 1 second) catch {
      case e: RuntimeException =>
        if (e.getMessage() != "Supressed")
          assert(false)
        // else ok
      // otherwise do not catch
    }
    assert(Await.result(f.continue(contThrow2), 1 second) === "2")
  }
  
  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }
  
  test("FutureCompanionOps.run test") {
    val p = Promise[String]()
    val working = Future.run() { ct =>
      Future {
        while (ct.nonCancelled) {
          //println("working")
        }
        //println("done")
        p.success("done")
      }
    }
    Future.delay(.5 seconds) onSuccess {
      case _ => working.unsubscribe()
    }
    assert(Await.result(p.future, 1 second) == "done")
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

  test("infinite running cancellation") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (s <- Stream.from(1).iterator) yield s.toString
    }
    Thread.sleep(200)
    val webpage = dummy.emit("/testDir", immutable.Map("11" -> List("11")))
    Await.result(Future.delay(0.5 second), 1 seconds)
    dummySubscription.unsubscribe()
    Thread.sleep(20)
    assert(webpage.loaded.future.now.startsWith("12345678910"))
  }

}
