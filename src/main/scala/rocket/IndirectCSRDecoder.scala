package freechips.rocketchip.rocket

import chisel3._
import org.chipsalliance.cde.config.Parameters

class IndirectCSRDecoder(
  xLen: Int,
  fullRanges: Seq[IndirectCSRRange],
  selectedRange: IndirectCSRRange
)(implicit p: Parameters) extends Module {
  private val selectedSlot = fullRanges.indexWhere(_.name == selectedRange.name)

  require(selectedSlot >= 0,
    s"selectedRange ${selectedRange.name} not found in fullRanges")

  val io = IO(new Bundle {
    val in  = Flipped(new IndirectCSRIO(xLen, fullRanges))
    val out = new IndirectCSRIO(xLen, Seq(selectedRange))
  })

  io.out.index := io.in.index
  io.out.reg   := io.in.reg
  io.out.wdata := io.in.wdata
  io.out.wen   := io.in.wen

  io.out.resp(0).hit   := false.B
  io.out.resp(0).rdata := 0.U

  for (r <- io.in.resp) {
    r.hit   := false.B
    r.rdata := 0.U
  }

  io.in.resp(selectedSlot).hit   := io.out.resp(0).hit
  io.in.resp(selectedSlot).rdata := io.out.resp(0).rdata
}