// RocketPerfEvents.scala
package freechips.rocketchip.rocket

case class PerfEventMeta(name: String, set: Int, bit: Int) {
  /*
   * This is the actual Rocket hardware mhpmevent selector value.
   *
   * This value is what OpenSBI should eventually write into mhpmeventN
   * when the user requests this as a raw event.
   */
  def encoding: BigInt = BigInt(set) | (BigInt(1) << (8 + bit))
}

object RocketPerfEvents {
  private val eventNames: Seq[Seq[String]] = Seq(
    Seq(
      "exception",
      "load",
      "store",
      "amo",
      "system",
      "arith",
      "branch",
      "jal",
      "jalr",
      "mul",
      "div",
      "fp load",
      "fp store",
      "fp add",
      "fp mul",
      "fp mul-add",
      "fp div/sqrt",
      "fp other"
    ),

    Seq(
      "load-use interlock",
      "long-latency interlock",
      "csr interlock",
      "I$ blocked",
      "D$ blocked",
      "branch misprediction",
      "control-flow target misprediction",
      "flush",
      "replay",
      "mul/div interlock",
      "fp interlock"
    ),

    Seq(
      "I$ miss",
      "D$ miss",
      "D$ release",
      "ITLB miss",
      "DTLB miss",
      "L2 TLB miss"
    ),

    Seq(
      "Ex PC Valid",
      "Mem PC Valid",
      "WB PC Valid",
      "Ex Reg Valid",
      "Mem Reg Valid",
      "WB Reg Valid",
      "IBuf valid",
      "IBuf cache-block",
      "ID stall",
      "Ctrl dependency",
      "Data dependency",
      "Stall due to mispr",
      "Data hazard ex",
      "Data hazard mem",
      "Data hazard wb",
      "IBuf ready",
      "Recovering"
    ),

    Seq(
      "uops issued",
      "Fetch Bubbles",
      "Recovering",
      "Fp stall",
      "Div stall"
    )
  )

  val eventSets: Seq[Seq[PerfEventMeta]] =
    eventNames.zipWithIndex.map { case (names, set) =>
      names.zipWithIndex.map { case (name, bit) =>
        PerfEventMeta(name, set, bit)
      }
    }

  val allEvents: Seq[PerfEventMeta] = eventSets.flatten

  /*
   * Raw Rocket hardware selector values.
   *
   * These are not SBI-standard event indexes. They are implementation-specific
   * mhpmevent selector/config values, so they should be emitted through
   * riscv,raw-event-to-mhpmcounters.
   */
  def rawPmuSelectors: Seq[BigInt] =
    allEvents.map(_.encoding).distinct.sorted

  /*
   * Deprecated compatibility alias.
   *
   * Use rawPmuSelectors for new code. This keeps older configs that call
   * RocketPerfEvents.pmuMappings from breaking, but the resource should now
   * treat the second field as the raw selector value, not as a standard SBI
   * event index.
   */
  def pmuMappings: Seq[(BigInt, BigInt)] =
    rawPmuSelectors.map(selector => selector -> selector)

  def names(set: Int): Seq[String] =
    eventSets(set).map(_.name)
}