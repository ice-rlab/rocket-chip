// See LICENSE.SiFive for license details.

package freechips.rocketchip.tile

import chisel3._
import chisel3.util.{isPow2}
import org.chipsalliance.cde.config._
import freechips.rocketchip.rocket._
import freechips.rocketchip.util._

case object CoreKey extends Field[CoreParams]
case object XLen extends Field[Int]
case object MaxHartIdBits extends Field[Int]

class RocketTraceBundle extends Bundle {
  val top_bit = Bool() // For trace sanity checking
  val top_second = Bool() // For trace sanity checking
  val recovering = Bool()
  val uops_issued = Bool()
  val fetch_bubbles = Bool()
  val fp_stall = Bool()
  val div_stall = Bool()
  val ibuf_valid = Bool()
  val ibuf_ready = Bool()
  val data_hazard_wb = Bool()
  val data_hazard_mem = Bool()
  val data_hazard_ex = Bool()
  val id_mem_hazard = Bool()
  val id_wb_hazard = Bool()
  val id_ex_hazard = Bool()
  val ctrl_stalld_w = Bool()
  val take_pc_mem_wb = Bool()
  val take_pc = Bool()
  val wb_reg_valid = Bool()
  val mem_reg_valid = Bool()
  val ex_reg_valid = Bool()
  val wb_pc_valid = Bool()
  val mem_pc_valid = Bool()
  val ex_pc_valid = Bool()
  val icache_miss = Bool()
  val dcache_miss = Bool()
  val dcache_release = Bool()
  val itlb_miss = Bool()
  val dtlb_miss = Bool()
  val flush = Bool()
  val replay = Bool()
  val control_flow_mispr = Bool()
  val branch_mispr = Bool()
  val dcache_blocked = Bool()
  val icache_blocked = Bool()
  val csr_interlock = Bool()
  val id_sboard_hazard = Bool()
  val id_vconfig_hazard = Bool()
  val rocc_busy = Bool()
  val id_do_fence = Bool()
  val load_use = Bool()
  val csr_stall = Bool()
  val id_reg_pause = Bool()
  val retire = Bool()
  val traceStall = Bool()
  val dmem_ready = Bool()
  val branch_instr = Bool()
  val jal = Bool()
  val jalr = Bool()
  val load = Bool()
  val store = Bool()
  val amo = Bool()
  val system = Bool()
  val mul = Bool()
  val div = Bool()
  val test_data = UInt((9 + 64).W) // For trace sanity checking
  val bottom_bit = Bool() // For trace sanity checking
}


// These parameters can be varied per-core
trait CoreParams {
  val bootFreqHz: BigInt
  val useVM: Boolean
  val useHypervisor: Boolean
  val useUser: Boolean
  val useSupervisor: Boolean
  val useDebug: Boolean
  val useAtomics: Boolean
  val useAtomicsOnlyForIO: Boolean
  val useCompressed: Boolean
  val useVector: Boolean = false
  val vectorUseDCache: Boolean = false
  val useRVE: Boolean
  val useConditionalZero: Boolean
  val useZba: Boolean
  val useZbb: Boolean
  val useZbs: Boolean
  val mulDiv: Option[MulDivParams]
  val fpu: Option[FPUParams]
  val fetchWidth: Int
  val decodeWidth: Int
  val retireWidth: Int
  val instBits: Int
  val nLocalInterrupts: Int
  val useNMI: Boolean
  val nPMPs: Int
  val pmpGranularity: Int
  val nBreakpoints: Int
  val useBPWatch: Boolean
  val mcontextWidth: Int
  val scontextWidth: Int
  val nPerfCounters: Int
  val haveBasicCounters: Boolean
  val haveFSDirty: Boolean
  val misaWritable: Boolean
  val haveCFlush: Boolean
  val nL2TLBEntries: Int
  val nL2TLBWays: Int
  val nPTECacheEntries: Int
  val mtvecInit: Option[BigInt]
  val mtvecWritable: Boolean
  val traceHasWdata: Boolean
  /* tracing for TMA testing*/
  // val enableDetailedTrace: Boolean

  val nLBREntries : Int = 0
  
  val xLen: Int
  val pgLevels: Int
  def traceCustom: Option[Data] = Some(new RocketTraceBundle)
  def customIsaExt: Option[String] = None
  def customCSRs(implicit p: Parameters): CustomCSRs = new CustomCSRs

  def hasSupervisorMode: Boolean = useSupervisor || useVM
  def instBytes: Int = instBits / 8
  def fetchBytes: Int = fetchWidth * instBytes
  def lrscCycles: Int

  def dcacheReqTagBits: Int = 6

  def minFLen: Int = 32

  def vLen: Int = 0
  def eLen: Int = 0
  def vfLen: Int = 0
  def vfh: Boolean = false
  def vExts: Seq[String] = Nil
  def hasV: Boolean = vLen >= 128 && eLen >= 64 && vfLen >= 64
  def vMemDataBits: Int = 0

  def useBitmanip = useZba && useZbb && useZbs
}

trait HasCoreParameters extends HasTileParameters {
  implicit val p: Parameters
  //def coreParams: CoreParams = p(TileKey).core
  def coreParams: CoreParams =
    if (p.lift(TileKey).isDefined && p(TileKey) != null) p(TileKey).core
    else if (p.lift(CoreKey).isDefined && p(CoreKey) != null) p(CoreKey)
    else null

  val minFLen = coreParams.fpu.map(_ => coreParams.minFLen).getOrElse(0)
  val fLen = coreParams.fpu.map(_.fLen).getOrElse(0)

  val usingMulDiv = coreParams.mulDiv.nonEmpty
  val usingFPU = coreParams.fpu.nonEmpty
  val usingAtomics = coreParams.useAtomics
  val usingAtomicsOnlyForIO = coreParams.useAtomicsOnlyForIO
  val usingAtomicsInCache = usingAtomics && !usingAtomicsOnlyForIO
  val usingCompressed = coreParams.useCompressed
  val usingVector = coreParams.useVector
  val usingNMI = coreParams.useNMI
  val usingConditionalZero = coreParams.useConditionalZero

  val retireWidth = coreParams.retireWidth
  val fetchWidth = coreParams.fetchWidth
  val decodeWidth = coreParams.decodeWidth

  val fetchBytes = coreParams.fetchBytes
  val coreInstBits = coreParams.instBits
  val coreInstBytes = coreInstBits/8
  val coreDataBits = xLen max fLen max vMemDataBits
  val coreDataBytes = coreDataBits/8
  def coreMaxAddrBits = paddrBits max vaddrBitsExtended

  val nBreakpoints = coreParams.nBreakpoints
  val nPMPs = coreParams.nPMPs
  val pmpGranularity = coreParams.pmpGranularity
  val nPerfCounters = coreParams.nPerfCounters
  val nLBREntries = coreParams.nLBREntries
  val mtvecInit = coreParams.mtvecInit
  val mtvecWritable = coreParams.mtvecWritable
  val customIsaExt = coreParams.customIsaExt
  val traceHasWdata = coreParams.traceHasWdata

  def vLen = coreParams.vLen
  def eLen = coreParams.eLen
  def vfLen = coreParams.vfLen
  def vMemDataBits = if (usingVector) coreParams.vMemDataBits else 0
  def maxVLMax = vLen

  if (usingVector) {
    require(isPow2(vLen), s"vLen ($vLen) must be a power of 2")
    require(eLen >= 32 && vLen % eLen == 0, s"eLen must divide vLen ($vLen) and be no less than 32")
    require(eLen == 32 || eLen == 64)
    require(vfLen <= eLen)
    require(!coreParams.vfh || (vfLen >= 32 && coreParams.minFLen <= 16))
  }

  if (coreParams.useVM) {
    if (coreParams.xLen == 32) {
      require(coreParams.pgLevels == 2)
    } else {
      require(coreParams.pgLevels >= 3)
    }
  }

  lazy val hartIdLen: Int = p(MaxHartIdBits)
  lazy val resetVectorLen: Int = {
    val externalLen = paddrBits
    require(externalLen <= xLen, s"External reset vector length ($externalLen) must be <= XLEN ($xLen)")
    require(externalLen <= vaddrBitsExtended, s"External reset vector length ($externalLen) must be <= virtual address bit width ($vaddrBitsExtended)")
    externalLen
  }

  // Print out log of committed instructions and their writeback values.
  // Requires post-processing due to out-of-order writebacks.
  val enableCommitLog = false

}

abstract class CoreModule(implicit val p: Parameters) extends Module
  with HasCoreParameters

abstract class CoreBundle(implicit val p: Parameters) extends ParameterizedBundle()(p)
  with HasCoreParameters

// This is a raw commit trace from the core, not the TraceCoreInterface
class TraceBundle(implicit val p: Parameters) extends Bundle with HasCoreParameters {
  val insns = Vec(coreParams.retireWidth, new TracedInstruction)
  val time = UInt(64.W)
  val custom = coreParams.traceCustom
}

