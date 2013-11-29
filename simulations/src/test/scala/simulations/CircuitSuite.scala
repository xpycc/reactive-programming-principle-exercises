package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  
  test("orGate example") {
    val  in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or 2")
    
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or 3")
    
    in1.setSignal(false)
    run
    assert(out.getSignal === true, "or 3")
    
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or 4")
  }
  
  test("orGate2 example") {
    val  in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or2 1")
    
    in1.setSignal(true)
    run
    assert(out.getSignal === true, "or2 2")
    
    in2.setSignal(true)
    run
    assert(out.getSignal === true, "or2 3")
    
    in1.setSignal(false)
    run
    assert(out.getSignal === true, "or2 3")
    
    in2.setSignal(false)
    run
    assert(out.getSignal === false, "or2 4")
  }

  test("demux example 1") {
    val in, out = new Wire
    demux(in, List(), List(out))
    in.setSignal(true)
    run
    assert(out.getSignal === true, "demux1 1")
    in.setSignal(false)
    run
    assert(out.getSignal === false, "demux1 2")
  }

  test("demux example 2") {
    val in = new Wire
    val c = (0 until 2).map(x => new Wire).toList
    val out = (0 until 4).map(x => new Wire).toList
    demux(in, c.reverse, out.reverse)
    var section = 0
    in.setSignal(true)
    c(0).setSignal(false)
    c(1).setSignal(false)
    run
    assert(out(0).getSignal === true,  s"demux2 $section.0")
    assert(out(1).getSignal === false, s"demux2 $section.1")
    assert(out(2).getSignal === false, s"demux2 $section.2")
    assert(out(3).getSignal === false, s"demux2 $section.3")

    section = 1
    c(0).setSignal(true)
    run
    assert(out(0).getSignal === false, s"demux2 $section.0")
    assert(out(1).getSignal === true,  s"demux2 $section.1")
    assert(out(2).getSignal === false, s"demux2 $section.2")
    assert(out(3).getSignal === false, s"demux2 $section.3")
    
    section = 3
    c(1).setSignal(true)
    run
    assert(out(0).getSignal === false, s"demux2 $section.0")
    assert(out(1).getSignal === false, s"demux2 $section.1")
    assert(out(2).getSignal === false, s"demux2 $section.2")
    assert(out(3).getSignal === true,  s"demux2 $section.3")

    section = 2
    c(0).setSignal(false)
    run
    assert(out(0).getSignal === false, s"demux2 $section.0")
    assert(out(1).getSignal === false, s"demux2 $section.1")
    assert(out(2).getSignal === true,  s"demux2 $section.2")
    assert(out(3).getSignal === false, s"demux2 $section.3")
    
    section = 2
    in.setSignal(false)
    run
    assert(out(0).getSignal === false, s"demux2 false $section.0")
    assert(out(1).getSignal === false, s"demux2 false $section.1")
    assert(out(2).getSignal === false, s"demux2 false $section.2")
    assert(out(3).getSignal === false, s"demux2 false $section.3")
    
    section = 0
    c(1).setSignal(false)
    run
    assert(out(0).getSignal === false, s"demux2 false $section.0")
    assert(out(1).getSignal === false, s"demux2 false $section.1")
    assert(out(2).getSignal === false, s"demux2 false $section.2")
    assert(out(3).getSignal === false, s"demux2 false $section.3")
    
    section = 1
    c(0).setSignal(true)
    run
    assert(out(0).getSignal === false, s"demux2 false $section.0")
    assert(out(1).getSignal === false, s"demux2 false $section.1")
    assert(out(2).getSignal === false, s"demux2 false $section.2")
    assert(out(3).getSignal === false, s"demux2 false $section.3")
    
    section = 3
    c(1).setSignal(true)
    run
    assert(out(0).getSignal === false, s"demux2 false $section.0")
    assert(out(1).getSignal === false, s"demux2 false $section.1")
    assert(out(2).getSignal === false, s"demux2 false $section.2")
    assert(out(3).getSignal === false, s"demux2 false $section.3")
  }
}
