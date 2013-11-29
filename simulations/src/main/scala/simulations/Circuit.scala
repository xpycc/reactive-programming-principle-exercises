package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val b1, b2, out = new Wire
    inverter(a1, b1)
    inverter(a2, b2)
    andGate(b1, b2, out)
    inverter(out, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    if (c.isEmpty) {
      val o = new Wire
      inverter(in, o)
      inverter(o, out.head)
    } else {
      val n = 1 << (c.size - 1)
      val (high, low) = out splitAt n
      val in0, in1 = new Wire
      val cv = new Wire
      inverter(c.head, cv)
      andGate(in, cv, in0)
      andGate(in, c.head, in1)
      demux(in0, c.tail, low)
      demux(in1, c.tail, high)
    }
  }
/*  {
    val n = c.size
    def setup(dep: Int, rest: List[Wire]) {
      def plug(dig: Int, restIn: List[Wire], in2:Wire): Wire = restIn match {
        case List() => in2
        case h :: t  => {
          val o = new Wire
          if ((dig & 1) != 0) {
            andGate(h, in2, o)
          } else {
            val hi = new Wire
            inverter(h, hi)
            andGate(hi, in2, o)
          }
          plug(dig >> 1, t, o)
        }
      }
      if (rest.isEmpty) ()
      else {
        val o = plug(dep, c.reverse, in)
        val to = new Wire
        inverter(o, to)
        inverter(to, rest.head)
        setup(dep - 1, rest.tail)
      }
    }
    setup((1 << n) - 1, out)
  }  */
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
  
  def orGateExample {
    val  in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    in1.setSignal(true)
    run
    
    in2.setSignal(true)
    run
    
    in1.setSignal(false)
    run
    
    in2.setSignal(false)
    run
  }

  def orGate2Example {
    val  in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    in1.setSignal(true)
    run
    
    in2.setSignal(true)
    run
    
    in1.setSignal(false)
    run
    
    in2.setSignal(false)
    run
  }
  
  def demuxExample1 {
    val in, out = new Wire
    demux(in, List(), List(out))
    probe("in", in)
    probe("out", out)
    in.setSignal(true)
    run
    in.setSignal(false)
    run
  }
  
  def demuxExample2 {
    val in, c0, out0, out1 = new Wire
    demux(in, List(c0), List(out1, out0))
    probe("in", in)
    probe("c0", c0)
    probe("out0", out0)
    probe("out1", out1)
    
    in.setSignal(true)
    c0.setSignal(false)
    run
    
    c0.setSignal(true)
    run
    
    in.setSignal(false)
    run
    
    c0.setSignal(false)
    run
  }
  
  def demuxExample3 {
    val in = new Wire
    val c = (0 until 2).map(x => new Wire).toList
    val out = (0 until 4).map(x => new Wire).toList
    demux(in, c.reverse, out.reverse)
    probe("in", in)
    for (i <- 0 until 2) { probe(s"c$i", c(i)) }
    for (i <- 0 until 4) { probe(s"out$i", out(i)) }
    in.setSignal(true)
    c(0).setSignal(false)
    c(1).setSignal(false)
    run
    
    c(0).setSignal(true)
    run
    
    c(1).setSignal(true)
    run
    
    c(0).setSignal(false)
    run
    
    in.setSignal(false)
    c(1).setSignal(false)
    run
    
    c(0).setSignal(true)
    run
    
    c(1).setSignal(true)
    run
    
    c(0).setSignal(false)
    run
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  println("AndGate")
  Circuit.andGateExample
  println("\nOrGate")
  Circuit.orGateExample
  println("\nOrGate2")
  Circuit.orGate2Example
  println("\nDemux 1")
  Circuit.demuxExample1
  println("\nDemux 2")
  Circuit.demuxExample2
  println("\nDemux 3")
  Circuit.demuxExample3
}
