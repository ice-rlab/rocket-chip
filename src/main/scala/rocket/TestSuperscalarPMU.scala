// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import chisel3.withClock
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property
import scala.collection.mutable.ArrayBuffer

// counter interface with event source
class SuperscalarCounterIO(NLanes: Int)(implicit p: Parameters) extends ParameterizedBundle {
  val inc = Input(Vec(NLanes, Bool()))
  //val read = Output(Vec(NLanes, Bool())) // TODO: read feedback
}

trait HasTestSuperscalarIO {
  implicit val p: Parameters

  val CoreWidth: Int = 3;
  val NCoreEvents: Int = 5;
  val IssueWidth: Int = 5;
  val NIssueEvents: Int = 2;
  val NEvents: Int = NCoreEvents + NIssueEvents;

  val io = IO(new ParameterizedBundle()(p) {
    // events from each core and issue lane
    val core_events  = Vec(NCoreEvents, new SuperscalarCounterIO(CoreWidth))
    val issue_events = Vec(NIssueEvents, new SuperscalarCounterIO(IssueWidth))

    // counter interface with software
    val reg_hpmevent = Input(Vec(NEvents, UInt(32.W)))
    val count        = Output(Vec(NEvents, Bits(CSR.hpmWidth.W)))
  })
}

abstract class AbstractSuperscalar(implicit val p: Parameters) extends Module {}

class TestSuperscalar(implicit p: Parameters) extends AbstractSuperscalar
  with HasTestSuperscalarIO {

  val clock_en_reg = RegInit(true.B)
  val clock_en = WireDefault(true.B)
  val gated_clock = clock

  class TestSuperscalarImpl { // entering gated-clock domain

    // event set implementation
    val perfEvents = new SuperscalarEventSets(Seq(
      (
        Seq.tabulate(CoreWidth)(core_i => // for each core
          new EventSet((mask, hits) => (mask & hits).orR,
            Seq.tabulate(NCoreEvents)(event_i => // for each event
              ("Event " + event_i, () => io.core_events(event_i).inc(core_i))
            )
          )
        ),
        (a, b) => { // accumulate sum of event signals in a cycle
          val a2 = Wire(UInt(log2Ceil(1+CoreWidth).W))
          val b2 = Wire(UInt(log2Ceil(1+CoreWidth).W))
          a2 := a
          b2 := b
          a2 + b2
        }
      ),
      (
        Seq.tabulate(IssueWidth)(issue_i => // for each issue
          new EventSet((mask, hits) => (mask & hits).orR,
            Seq.tabulate(NIssueEvents)(event_i => // for each event
              ("Event " + event_i, () => io.issue_events(event_i).inc(issue_i))
            )
          )
        ),
        (a, b) => { // accumulate sum of event signals in a cycle
          val a2 = Wire(UInt(log2Ceil(1+IssueWidth).W))
          val b2 = Wire(UInt(log2Ceil(1+IssueWidth).W))
          a2 := a
          b2 := b
          a2 + b2
        }
      ),
    ))

    // counter control registers
    val counter_inc = Wire(Vec(NEvents, UInt(log2Ceil(32).W)))
    val counter_event_sel = Wire(Vec(NEvents, UInt(32.W)))

    // counter implementation
    (counter_event_sel zip io.reg_hpmevent) foreach { case (c, e) => c := e }
    val reg_hpmcounter = counter_inc.map { c =>
      WideCounter(CSR.hpmWidth, c, reset = false, inhibit = false.B) }

    // connect events to counters
    (counter_inc zip counter_event_sel) foreach { case (c, s) => c := RegNext(perfEvents.evaluate(s)) }

    // output control
    reg_hpmcounter.zipWithIndex.map { case(c, i) =>
      io.count(i) := c.value }

  } // leaving gated-clock domain
  val testSuperscalarImpl = withClock (gated_clock) { new TestSuperscalarImpl }
}
