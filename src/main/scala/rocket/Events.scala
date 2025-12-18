// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.{Cat, log2Ceil}
import freechips.rocketchip.util._
import freechips.rocketchip.util.property

/* Superscalar aggregation mode */
object TopdownPMUMode {
  val NONE = 0                  // Do not track top-down events
  val SCALAR_COUNTERS = 1       // Count events separately
  val ADD_WIRES = 2             // Aggregate each separate event into a multi-bit increment signal
  val DISTRIBUTED_COUNTERS = 3  // Use local counter and arbitrate each overflow as increment
}

class EventSet(val gate: (UInt, UInt) => Bool, val events: Seq[(String, () => Bool)]) {
  def size = events.size
  val hits = WireDefault(VecInit(Seq.fill(size)(false.B)))
  def check(mask: UInt) = {
    hits := events.map(_._2())
    gate(mask, hits.asUInt)
  }
  def dump(): Unit = {
    for (((name, _), i) <- events.zipWithIndex)
      when (check(1.U << i)) { printf(s"Event $name\n") }
  }
  def withCovers: Unit = {
    events.zipWithIndex.foreach {
      case ((name, func), i) => property.cover(gate((1.U << i), (func() << i)), name)
    }
  }
}

class EventSets(val eventSets: Seq[EventSet]) {
  def maskEventSelector(eventSel: UInt): UInt = {
    // allow full associativity between counters and event sets (for now?)
    val setMask = (BigInt(1) << eventSetIdBits) - 1
    val maskMask = ((BigInt(1) << eventSets.map(_.size).max) - 1) << maxEventSetIdBits
    eventSel & (setMask | maskMask).U
  }

  private def decode(counter: UInt): (UInt, UInt) = {
    require(eventSets.size <= (1 << maxEventSetIdBits))
    require(eventSetIdBits > 0)
    (counter(eventSetIdBits-1, 0), counter >> maxEventSetIdBits)
  }

  def evaluate(eventSel: UInt): Bool = {
    val (set, mask) = decode(eventSel)
    val sets = for (e <- eventSets) yield {
      require(e.hits.getWidth <= mask.getWidth, s"too many events ${e.hits.getWidth} wider than mask ${mask.getWidth}")
      e check mask
    }
    sets(set)
  }

  def cover() = eventSets.foreach { _.withCovers }

  private def eventSetIdBits = log2Ceil(eventSets.size)
  private def maxEventSetIdBits = 8

  require(eventSetIdBits <= maxEventSetIdBits)
}

class SuperscalarEventSets(val eventSets: Seq[(Seq[EventSet], (UInt, UInt) => UInt)]) {
  def maskEventSelector(eventSel: UInt): UInt = {
    // allow full associativity between counters and event sets (for now?)
    val setMask = (BigInt(1) << eventSetIdBits) - 1
    val maskMask = ((BigInt(1) << (eventSets.map { case (sets, _) =>
      sets(0).size
    }).max) - 1) << maxEventSetIdBits
    eventSel & (setMask | maskMask).U
  }

  def evaluate(eventSel: UInt): UInt = {
    val (set, mask) = decode(eventSel)
    val sets = for ((sets, reducer) <- eventSets) yield {
      sets.map { set =>
        require(set.hits.getWidth <= mask.getWidth, s"too many events ${set.hits.getWidth} wider than mask ${mask.getWidth}")
        set.check(mask)
      }.reduce(reducer)
    }
    val zeroPadded = sets.padTo(1 << eventSetIdBits, 0.U)
    zeroPadded(set)
  }

  def toScalarEventSets: EventSets = new EventSets(eventSets.map(_._1.head))

  def cover(): Unit = { eventSets.foreach(_._1.foreach(_.withCovers)) }

  private def decode(counter: UInt): (UInt, UInt) = {
    require(eventSets.size <= (1 << maxEventSetIdBits))
    require(eventSetIdBits > 0)
    (counter(eventSetIdBits-1, 0), counter >> maxEventSetIdBits)
  }

  private def eventSetIdBits = log2Ceil(eventSets.size)
  private def maxEventSetIdBits = 8

  require(eventSets.forall(s => s._1.forall(_.size == s._1.head.size)))
  require(eventSetIdBits <= maxEventSetIdBits)
}

class AddWiresEventSets(val eventSets: Seq[Seq[EventSet]]) {
  def toSuperscalarEventSets: SuperscalarEventSets = new SuperscalarEventSets(eventSets.map { case set =>
    (
      set,
      (a: UInt, b: UInt) => { // accumulate sum of event signals
        val a2 = Wire(UInt(log2Ceil(1+eventSets.size).W))
        val b2 = Wire(UInt(log2Ceil(1+eventSets.size).W))
        a2 := a
        b2 := b
        a2 + b2
      }
    )
  })
}

class DistributedCountersEventSets(val eventSets: Seq[Seq[EventSet]], reset: Bool) {
  def toSuperscalarEventSets: SuperscalarEventSets = new SuperscalarEventSets(eventSets.map { case seq =>
    if (seq.size == 1) {
      (
        seq,
        (a: UInt, b: UInt) => a + b
      )
    } else {

      val n_sources = seq.size
      val n_events = seq.head.size
      val ctr_width = if (n_sources == 1) 1 else log2Ceil(n_sources)

      // one barrel shifter for each set of superscalar events from the same sources
      val counter_ack = Reg(UInt(n_sources.W))
      when (reset) {
        counter_ack := 1.U(n_sources.W)
      } .otherwise {
        counter_ack := Cat(counter_ack(n_sources-2,0), counter_ack(n_sources-1))
      }
      (
        Seq.tabulate(seq.length) ( x =>
          new EventSet((mask, hits) => (mask & hits).orR, seq(x).events.map { case e =>
            val ctr = freechips.rocketchip.util.WideCounterOverflow(
              ctr_width, e._2().asUInt, false, false.B, counter_ack(x))
            (
              e._1,
              () => ctr.overflow & counter_ack(x)
            )
          })
        ),
        (a: UInt, b: UInt) => a + b
      )
    }
  })
}

class TopdownEventSets(val pmuMode: Int, val eventSets: Seq[Seq[EventSet]], reset: Bool) {
  def toSuperscalarEventSets: SuperscalarEventSets = {
    pmuMode match {
      case TopdownPMUMode.NONE =>
        new SuperscalarEventSets(eventSets.map { case sets =>
          var flattened = Seq[(String, () => chisel3.Bool)]()
          sets foreach { set =>
            flattened = flattened ++ set.events
          }
          (
            Seq(new EventSet(
              (mask, hits) => (mask & hits).orR,
              flattened
            )),
            (a: UInt, b: UInt) => a + b
          )
        })
      case TopdownPMUMode.SCALAR_COUNTERS =>
        new SuperscalarEventSets(eventSets.map { case sets =>
          var flattened = Seq[(String, () => chisel3.Bool)]()
          sets foreach { set =>
            flattened = flattened ++ set.events
          }
          (
            Seq(new EventSet(
              (mask, hits) => (mask & hits).orR,
              flattened
            )),
            (a: UInt, b: UInt) => a + b
          )
        })
      case TopdownPMUMode.ADD_WIRES =>
        new AddWiresEventSets(eventSets).toSuperscalarEventSets
      case TopdownPMUMode.DISTRIBUTED_COUNTERS =>
        new DistributedCountersEventSets(eventSets, reset).toSuperscalarEventSets
      case _ =>
        null
    }
  }
}
