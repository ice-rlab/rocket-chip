package freechips.rocketchip.resources

import scala.collection.immutable.ListMap

class PerfEventsResource(
  rawMhpmeventSelectors: Seq[BigInt],
  counterMask: BigInt,
  selectorMask: BigInt = BigInt("ffffffffffffffff", 16)
) extends Device {
  override def parent: Option[Device] = None

  private def hi32(x: BigInt): BigInt =
    (x >> 32) & BigInt("ffffffff", 16)

  private def lo32(x: BigInt): BigInt =
    x & BigInt("ffffffff", 16)

  override def describe(resources: ResourceBindings): Description = {
    val rawSelectors = rawMhpmeventSelectors.distinct.sorted

    Description("pmu", ListMap(
      "compatible" -> Seq(ResourceString("riscv,pmu")),
      "status" -> Seq(ResourceString("okay")),

      /*
       * OpenSBI format:
       *
       * riscv,raw-event-to-mhpmcounters =
       *   <raw_selector_hi raw_selector_lo
       *    selector_mask_hi selector_mask_lo
       *    counter_mask>;
       *
       * This allows Linux/OpenSBI raw PMU events to program the same
       * Rocket mhpmevent selector values without pretending they are
       * standard SBI event indexes.
       */
      "riscv,raw-event-to-mhpmcounters" ->
        rawSelectors.flatMap { selector =>
          Seq(
            ResourceInt(hi32(selector)),
            ResourceInt(lo32(selector)),
            ResourceInt(hi32(selectorMask)),
            ResourceInt(lo32(selectorMask)),
            ResourceInt(counterMask)
          )
        }
    ))
  }
}

object PerfEventsResource {
  private def hpmCounterMask(nPerfCounters: Int): BigInt = {
    require(nPerfCounters >= 0)
    require(nPerfCounters <= 29)

    if (nPerfCounters == 0) BigInt(0)
    else ((BigInt(1) << nPerfCounters) - 1) << 3
  }

  /*
   * Preferred new API.
   *
   * Pass RocketPerfEvents.rawPmuSelectors here.
   */
  def bindRaw(
    rawMhpmeventSelectors: Seq[BigInt],
    nPerfCounters: Int
  ): Unit = {
    require(rawMhpmeventSelectors.nonEmpty, "PMU raw event selector list cannot be empty")

    val pmu = new PerfEventsResource(
      rawMhpmeventSelectors = rawMhpmeventSelectors,
      counterMask = hpmCounterMask(nPerfCounters)
    )

    ResourceBinding {
      Resource(pmu, "status").bind(ResourceString("okay"))
    }
  }

  /*
   * Backwards-compatible API.
   *
   * Old call sites may pass Seq[(eventId, mhpmEvent)].
   * For Rocket custom events, we ignore the old logical eventId and use
   * mhpmEvent as the raw selector.
   */
  def bind(
    eventToMhpmevent: Seq[(BigInt, BigInt)],
    nPerfCounters: Int
  ): Unit = {
    require(eventToMhpmevent.nonEmpty, "PMU event list cannot be empty")

    val rawSelectors = eventToMhpmevent.map { case (_, mhpmEvent) => mhpmEvent }

    bindRaw(
      rawMhpmeventSelectors = rawSelectors,
      nPerfCounters = nPerfCounters
    )
  }
}