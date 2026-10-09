// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import org.chipsalliance.cde.config.{Config, Field}

/** Filter tag for the PrefetchProf events that come from a tile's prefetcher (prefetch issue, prefetch fills, the
  * prefetched-line lifetime events). Empty (the default) means untagged: the events are always recorded.
  *
  * With a tag, the events are only recorded while the tag is selected at run time with the driver plusarg
  * `+prefetchprof-filter=<tag>[,<tag>...]` (every tag in the build is selected by default). Tag the prefetchers of
  * different configurations differently to evaluate several in one build.
  */
case object PrefetchProfTagKey extends Field[String]("")

/** Sets the PrefetchProf filter tag of the prefetcher events, e.g. `new WithPrefetchProfTag("strided") ++ ...`. */
class WithPrefetchProfTag(tag: String) extends Config((site, here, up) => { case PrefetchProfTagKey => tag })

/** Tile ids whose L1D has an external prefetch port (see typeawareprefetcher.WithExternalPrefetchPort). Requests from
  * that port carry the reserved requestor index dcacheArbPorts-1 in their low tag bits. PrefetchProf uses this to
  * classify them as prefetches instead of core demand accesses. Empty (the default) leaves every count unchanged.
  */
case object ExternalPrefetchPortKey extends Field[Seq[Int]](Seq())
