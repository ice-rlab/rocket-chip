// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util.log2Ceil
import freechips.rocketchip.util._
import freechips.rocketchip.util.property

class EventSet(
    val gate: (UInt, UInt) => Bool,
    val events: Seq[(String, () => UInt)]
) {
  def size: Int = events.size

  // Raw event values. These may have different widths.
  def values: Seq[UInt] = events.map { case (_, func) => func() }

  def hits: UInt = {
    VecInit(values.map(_.orR)).asUInt
  }

  def check(mask: UInt): Bool = {
    gate(mask, hits)
  }

  def count(mask: UInt): UInt = {
    values.zipWithIndex
      .map { case (value, i) =>
        Mux(mask(i), value, 0.U)
      }
      .reduceOption(_ +& _)
      .getOrElse(0.U)
  }

  def dump(): Unit = {
    for (((name, _), i) <- events.zipWithIndex) {
      when(check(1.U << i)) {
        printf(s"Event $name\n")
      }
    }
  }

  def withCovers: Unit = {
    events.zipWithIndex.foreach { case ((name, func), i) =>
      property.cover(gate(1.U << i, func().orR.asUInt << i), name)
    }
  }
}

class EventSets(val eventSets: Seq[EventSet]) {
  def maskEventSelector(eventSel: UInt): UInt = {
    // allow full associativity between counters and event sets (for now?)
    val setMask = (BigInt(1) << eventSetIdBits) - 1
    val maskMask =
      ((BigInt(1) << eventSets.map(_.size).max) - 1) << maxEventSetIdBits

    val ofMask =
      if (eventSel.getWidth == 64) (BigInt(1) << 63)
      else BigInt(0)

    eventSel & (setMask | maskMask | ofMask).U
  }

  private def decode(counter: UInt): (UInt, UInt) = {
    val set = counter(eventSetIdBits - 1, 0)
    val mask = counter >> maxEventSetIdBits
    (set, mask)
  }

  def evaluate(eventSel: UInt): UInt = {
    val (set, mask) = decode(eventSel)
    eventSets.zipWithIndex
      .map { case (eventSet, index) =>
        (set === index.U) && eventSet.check(mask)
      }
      .reduce(_ || _)
  }

  def cover() = eventSets.foreach { _.withCovers }

  private def eventSetIdBits = log2Ceil(eventSets.size)
  private def maxEventSetIdBits = 8

  require(eventSetIdBits <= maxEventSetIdBits)
}