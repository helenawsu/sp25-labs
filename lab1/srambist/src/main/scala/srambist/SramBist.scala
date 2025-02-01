package srambist


import chisel3._
import chisel3.util.log2Ceil


/** Access patterns that can be exercised by the BIST.
 *
 *   W0 - Write `data0` to each address sequentially, starting from address 0.
 *   W1 - Write `data1` to each address sequentially, starting from address 0.
 *   R0 - Reads each address sequentially and validates that it equals `data0`,
 *        starting from address 0.
 *   R1 - Reads each address sequentially and validates that it equals `data1`,
 *        starting from address 0.
 */
object Pattern extends Enumeration {
 type Type = Value
 val W0, W1, R0, R1 = Value
}


/** Runs a set of patterns against an SRAM to verify its behavior.
 *
 * Once the BIST is complete, `done` should be set high and remain high. If the BIST
 * failed, `fail` should be asserted on the same cycle and remain high.
 */
class SramBist(numWords: Int, dataWidth: Int, patterns: Seq[Pattern.Type])
   extends Module {
 val io = IO(new Bundle {
   // SRAM interface
   val we = Output(Bool())
   val addr = Output(UInt(log2Ceil(numWords).W))
   val din = Output(UInt(dataWidth.W))
   val dout = Input(UInt(dataWidth.W))


   // BIST interface
   val data0 = Input(UInt(dataWidth.W))
   val data1 = Input(UInt(dataWidth.W))
   val done = Output(Bool())
   val fail = Output(Bool())
 })
 val count = RegInit(0.U(log2Ceil(numWords + 1).W)) // count how many words needs to be read / written for each pattern
 val patternCount = RegInit(0.U(log2Ceil(patterns.length + 1).W)) // count how many patterns in list so i can set done to true
 val failReg = RegInit(false.B)
 val doneReg = RegInit(false.B)
 val checkReg = RegInit(false.B)
 val weReg = RegInit(false.B)
 val addrReg = RegInit(0.U(log2Ceil(numWords).W))
 val dinReg = RegInit(0.U(dataWidth.W))
 io.we := weReg
 io.addr := addrReg
 io.din := dinReg
 io.done := doneReg
 io.fail := failReg
 patterns.zipWithIndex.foreach { // A Scala `foreach` loop to iterate over the Scala sequence `patterns`.
 case (pattern, idx) => {
   when(patternCount === idx.U) { // Chisel `when` statement to match against the hardware register `ctr`.
     when(count < (numWords.U + 2.U)) { // last count to check for read, do nothing for write
     pattern match { // Scala `match` statement to match against a specific Scala `Pattern` enumeration.
       case Pattern.W0 => {
         weReg := true.B
         checkReg := false.B
         when(count === numWords.U){
           weReg := false.B
         }
         addrReg := count
         dinReg := io.data0
       }
       case Pattern.W1 => {
         weReg := true.B
         checkReg := false.B
         when(count === numWords.U){
           weReg := false.B
         }
         addrReg := count
         dinReg := io.data1
       }
       case Pattern.R0 => {
         weReg := false.B
         addrReg := count
         checkReg := true.B
         dinReg := io.data0
         when(checkReg === true.B && io.dout =/= io.data0) {
           failReg := true.B
           doneReg := true.B
         }
         when(count === numWords.U + 1.U) {
           checkReg := false.B
         }
       }
       case Pattern.R1 => {
         weReg := false.B
         addrReg := count
         checkReg := true.B
         dinReg := io.data1
         when(checkReg === true.B && io.dout =/= io.data1) {
           failReg := true.B
           doneReg := true.B
         }
         when(count === numWords.U + 1.U) {
           checkReg := false.B
         }
       }
     }
     count := count + 1.U
   }
   }
 }
 };
when(count === numWords.U + 1.U) {
 count := 0.U
 patternCount := patternCount + 1.U
}
 when (patternCount === patterns.length.U ){
   doneReg := true.B
 }
}
