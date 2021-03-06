/*
 Copyright (c) 2011, 2012, 2013, 2014 The Regents of the University of
 California (Regents). All Rights Reserved.  Redistribution and use in
 source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

    * Redistributions of source code must retain the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer.
    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      two paragraphs of disclaimer in the documentation and/or other materials
      provided with the distribution.
    * Neither the name of the Regents nor the names of its contributors
      may be used to endorse or promote products derived from this
      software without specific prior written permission.

 IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
 SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
 ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
 REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
 ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
 TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 MODIFICATIONS.
*/

package Chisel

import com.gilt.handlebars.scala.Handlebars
import com.gilt.handlebars.scala.binding.dynamic._
import scala.collection.mutable
import scala.sys.process._


object VerilogBackend {
  val keywords = Set[String](
    "always", "and", "assign", "attribute", "begin", "buf", "bufif0", "bufif1",
    "case", "casex", "casez", "cmos", "deassign", "default", "defparam",
    "disable", "edge", "else", "end", "endattribute", "endcase", "endfunction",
    "endmodule", "endprimitive", "endspecify", "endtable", "endtask", "event",
    "for", "force", "forever", "fork", "function", "highz0", "highz1", "if",
    "ifnone", "initial", "inout", "input", "integer", "initvar", "join",
    "medium", "module", "large", "macromodule", "nand", "negedge", "nmos",
    "nor", "not", "notif0", "notif1", "or", "output", "parameter", "pmos",
    "posedge", "primitive", "pull0", "pull1", "pulldown", "pullup", "rcmos",
    "real", "realtime", "reg", "release", "repeat", "rnmos", "rpmos", "rtran",
    "rtranif0", "rtranif1", "scalared", "signed", "small", "specify",
    "specparam", "strength", "strong0", "strong1", "supply0", "supply1",
    "table", "task", "time", "tran", "tranif0", "tranif1", "tri", "tri0",
    "tri1", "triand", "trior", "trireg", "unsigned", "vectored", "wait",
    "wand", "weak0", "weak1", "while", "wire", "wor", "xnor", "xor", "rand", "fd",
    "SYNTHESIS", "PRINTF_COND", "VCS")
}

abstract class Simulator extends FileSystemUtilities {
  def compile (c: Module, n: String, flags: Option[String]): Unit
  def vcdHarness(modName:String , isMem: Boolean): Seq[String]
  def target(c:Module, n: String): Seq[String]

  def moduleSources(n: String) = Seq(n + ".v", n + "-harness.v")

  val extraSources = mutable.ListBuffer.empty[String]

  val dir = Driver.targetDir
  val vpiName = "vpi"

  val clkPeriod = 1

  def simSources(n: String): Seq[String] = extraSources ++ moduleSources(n)
}

class IVerilog extends Simulator{
  def compile (c: Module, n: String, flags: Option[String]) = {
    val vvpName = n + ".vvp"
    cc(dir, vpiName, "-std=c++11 " + ("iverilog-vpi --cflags".!!).trim)
    val vpiCmd = List("cd", dir, "&&", "iverilog-vpi", vpiName + ".o", s"--name=$vpiName").mkString(" ")
    if (!run(vpiCmd)) throwException("failed to run iverilog-vpi" + vpiName + ".cpp")

    val cmd = List("cd", dir, "&&" ,"iverilog", "-mvpi", s"-o$vvpName",
      "-tvvp", flags.getOrElse(""), "-DCLOCK_PERIOD=" + clkPeriod, simSources(n) mkString " " ).mkString(" ")
    if (!run(cmd)) throwException("failed to run iverilog-vpi for " + n)

//    val vvpFile = scala.io.Source.fromFile(dir + vvpName)
//    val exeName = dir + "/" + name
//    val exe = new PrintWriter(exeName)
//    exe.println(s"#! /usr/local/bin/vvp -M./$dir")
//    vvpFile.getLines().drop(1).foreach(exe.println(_))
//    exe.close()
//    new java.io.File(exeName).setExecutable(true)
  }

  def vcdHarness(modName:String , isMem: Boolean) = {
    Seq("/*** VCD dump ***/", "$dumpfile(\"%s.vcd\");".format(dir + modName), "$dumpvars(0);") ++
      Seq(if (isMem) "/*$dumpmem();///???*/" else "")
  }

  def target(c:Module, n: String) =  Seq("vvp", s"-M$dir", s"$dir$n.vvp")
}

/**
 *  support for Mentor Graphics Modelsim and Questasim
 *  MGC_HOME environment variable should point to installation folder
 *  and $$MGC_HOME/bin should be in PATH
 */

class Modelsim extends Simulator{
  val workLibName = "work"

  def compile (c: Module, n: String, flags: Option[String]) = {

    val ccFlags = List("-I$MGC_HOME/include", "-I" + dir, "-fPIC", "-std=c++11")
    cc(dir, vpiName, ccFlags mkString " ")
    link(dir, vpiName + ".so", List(vpiName + ".o"), isLib=true)
    var cmd = List("cd", dir, "&&" ,s"vlib $workLibName").mkString(" ")
    if (!run(cmd)) throwException(s"vlib $workLibName failed!")

    cmd = List("cd", dir, "&&" ,s"vlog -64 +define+CLOCK_PERIOD=$clkPeriod " +
      " -timescale \"1ns / 1ps\" -vopt -quiet " + (simSources(n) mkString " ")).mkString(" ")
    if (!run(cmd)) throwException(s"$cmd FAILED!")
  }

  def vcdHarness(modName:String, isMem: Boolean) = {
    Seq( "/*** VCD dump ***/", "$fdumpfile(\"%s.vcd\");".format(dir + modName), "$dumpvars(0);") ++
      Seq(if (isMem) "/*$dumpmem();///???*/" else "") // TODO
  }


  def target(c:Module, n: String) = Seq("vsim", "-64", "-noautoldlibpath", "-vopt", "-quiet", "-c",
    "-do", s"cd $dir; onElabError resume; run -all; exit", "-pli", s"$dir/$vpiName.so", "-lib", s"$dir/$workLibName",
    "test")
}

class Verilator extends Simulator{

  override def moduleSources(n: String) = Seq(n + ".v", n + "-harness.cpp")
  def compile (c: Module, n: String, flags: Option[String]) = {
    val cmd = List("cd", dir, "&&" ,"verilator", "--top-module", c.moduleName, "-Wno-WIDTH",
      "-CFLAGS -std=c++11", "-Wno-STMTDLY", "--exe", "--cc",
      flags.getOrElse(""), if(Driver.isVCD) "--trace" else "" , "+define+CLOCK_PERIOD=" + clkPeriod, simSources(n) mkString " " ).mkString(" ")
    if (!run(cmd)) throwException(s"failed to run $cmd for $n")

    val makeCmd = List("cd", dir + "obj_dir", "&&" ,"make", "-f", s"V${c.moduleName}.mk").mkString(" ")
    if (!run(makeCmd)) throwException(s"failed to run $cmd for $n")
  }

  def vcdHarness(modName:String , isMem: Boolean) = {
    Seq("/*** VCD dump ***/", "$dumpfile(\"%s.vcd\");".format(dir + modName), "$dumpvars(0);") ++
      Seq(if (isMem) "/*$dumpmem();///???*/" else "") // TODO find a way to dump memory in iverilog
  }


  def target(c:Module, n: String) = Seq(dir +"obj_dir/V" + c.moduleName)
}

class Vcs extends  Simulator {
  def compile (c: Module, n: String, flags: Option[String] = None) = {
    val ccFlags = List("-I$VCS_HOME/include", "-I" + dir, "-fPIC", "-std=c++11")
    cc(dir, "vpi", ccFlags mkString " ")
    link(dir, "vpi.so", List("vpi.o"), isLib=true)

    val vcsFlags = List("-full64", "-quiet", "-timescale=1ns/1ps", "-debug_pp", "-Mdir=" + n + ".csrc",
      "+v2k", "+vpi", "+define+CLOCK_PERIOD=" + clkPeriod, "+vcs+initreg+random")
    val cmd = (List("cd", dir, "&&", "vcs") ++ vcsFlags ++ List("-use_vpiobj", "vpi.so", "-o", n) ++
      simSources(n)) mkString " "
    if (!run(cmd)) throw new RuntimeException("vcs command failed")
  }

  def vcdHarness(modName:String, isMem: Boolean) = {
    Seq("/*** VPD dump ***/", "$vcdplusfile(\"%s.vpd\");".format(dir + modName), "$vcdpluson(0);") ++
    Seq(if(isMem) "$vcdplusmemon;" else "")
  }

  def target(c:Module, n: String) = Seq(dir + n, "-q", "+vcs+initreg+0")
}

class VerilogBackend(simulatorName: String = "vcs", verilogExtraSources: List[String] = List.empty[String]) extends Backend {
  val keywords = VerilogBackend.keywords
  override val needsLowering = Set("PriEnc", "OHToUInt", "Log2")

  override def isEmittingComponents: Boolean = true

  val emittedModules = mutable.HashSet[String]()

  val memConfs = mutable.HashMap[String, String]()
  val compIndices = mutable.HashMap[String, Int]()

  val simulator = simulatorName match  {
    case "vcs" => new Vcs
    case "iverilog" => new IVerilog
    case "verilator" => new Verilator
    case "modelsim" | "questasim" => new Modelsim
    case _ => Class.forName("Chisel."+simulatorName).newInstance.asInstanceOf[Simulator]
  }

  private def getMemConfString: String =
    memConfs.map { case (conf, name) => "name " + name + " " + conf } reduceLeft(_ + _)

  private def getMemName(mem: Mem[_], configStr: String): String = {
    if (!memConfs.contains(configStr)) {
      /* Generates memory that are different in (depth, width, ports).
       All others, we return the previously generated name. */
      val compName = (if( !mem.component.moduleName.isEmpty ) {
        Driver.moduleNamePrefix + mem.component.moduleName
      } else {
        extractClassName(mem.component)
      }) + "_"
      // Generate a unique name for the memory module.
      val candidateName = compName + emitRef(mem)
      val memModuleName = if( compIndices contains candidateName ) {
        val count = compIndices(candidateName) + 1
        compIndices += (candidateName -> count)
        candidateName + "_" + count
      } else {
        compIndices += (candidateName -> 0)
        candidateName
      }
      memConfs += (configStr -> memModuleName)
    }
    memConfs(configStr)
  }

  def emitWidth(node: Node): String = {
    val w = node.needWidth()
    if (w == 1) "" else "[" + (w-1) + ":0]"
  }

  override def emitTmp(node: Node): String =
    emitRef(node)

  override def emitRef(node: Node): String = {
    node match {
      case x: Literal => emitLit(x.value, x.needWidth())
      case _: Reg =>
        if (node.name != "") node.name else "R" + node.emitIndex
      case _ =>
        if (node.name != "") node.name else "T" + node.emitIndex
    }
  }

  private def emitLit(x: BigInt): String =
    emitLit(x, x.bitLength + (if (x < 0) 1 else 0))
  private def emitLit(x: BigInt, w: Int): String = {
    val unsigned = if (x < 0) (BigInt(1) << w) + x else x
    require(unsigned >= 0)
    w + "'h" + unsigned.toString(16)
  }

  // $random only emits 32 bits; repeat its result to fill the Node
  private def emitRand(node: Node): String =
    "{" + ((node.needWidth()+31)/32) + "{$random}}"

  def emitPortDef(m: MemAccess, idx: Int): String = {
    def str(prefix: String, ports: (String, Option[String])*): String =
      ports.toList filter (_._2.isDefined) map {
        case (x, y) => List("    .", prefix, idx, x, "(", y.get, ")").mkString 
      } mkString ",\n"

    m match {
      case r: MemSeqRead =>
        val addr = ("A", Some(emitRef(r.addr)))
        val en = ("E", Some(emitRef(r.cond)))
        val out = ("O", Some(emitTmp(r)))
        str("R", addr, en, out)

      case w: MemWrite =>
        val addr = ("A", Some(emitRef(w.addr)))
        val en = ("E", Some(emitRef(w.cond)))
        val data = ("I", Some(emitRef(w.data)))
        val mask = ("M", if (w.isMasked) Some(emitRef(w.mask)) else None)
        str("W", addr, en, data, mask)

      case rw: MemReadWrite =>
        val (r, w) = (rw.read, rw.write)
        val addr = ("A", Some(emitRef(w.cond) + " ? " + emitRef(w.addr) + " : " + emitRef(r.addr)))
        val en = ("E", Some(emitRef(r.cond) + " || " + emitRef(w.cond)))
        val write = ("W", Some(emitRef(w.cond)))
        val data = ("I", Some(emitRef(w.data)))
        val mask = ("M", if (w.isMasked) Some(emitRef(w.mask)) else None)
        val out = ("O", Some(emitTmp(r)))
        str("RW", addr, en, write, data, mask, out)
    }
  }

  def emitDef(c: Module): StringBuilder = {
    val spacing = if(c.verilog_parameters != "") " " else ""
    val res = new StringBuilder
    List("  ", c.moduleName, " ", c.verilog_parameters, spacing, c.name, "(") addString res
    def getMapClk(x : Clock) : String = c match {
      case c:BlackBox => c.mapClock(emitRef(x))
      case _ => emitRef(x)
    }
    c.clocks map (x => "." + getMapClk(x) + "(" + emitRef(x) + ")") addString (res, ", ")
    if (c.clocks.nonEmpty && c.resets.nonEmpty) res append ", "
    c.resets.values map (x => "." + emitRef(x) + "(" + emitRef(x.inputs(0)) + ")") addString (res, ", ")
    var isFirst = true
    val portDecs = mutable.ArrayBuffer[StringBuilder]()
    for ((n, io) <- c.wires if n != "reset" && n != Driver.implicitReset.name) {
      val portDec = new StringBuilder("." + n + "( ")
      io.dir match {
        case INPUT if io.inputs.isEmpty =>
          // if (Driver.saveConnectionWarnings) {
          //   ChiselError.warning("" + io + " UNCONNECTED IN " + io.component)
          // } removed this warning because pruneUnconnectedIOs should have picked it up
          portDec insert (0, "//")
        case INPUT if io.inputs.size > 1 =>
          if (Driver.saveConnectionWarnings) {
            ChiselError.warning("" + io + " CONNECTED TOO MUCH " + io.inputs.length)
          }
          portDec insert (0, "//")
        /* case INPUT if !(c.isWalked conatins io) =>
          if (Driver.saveConnectionWarnings) {
            ChiselError.warning(" UNUSED INPUT " + io + " OF " + c + " IS REMOVED")
          }
          portDec = "//" + portDec // I don't think this is necessary */
        case INPUT =>
          portDec append emitRef(io.inputs(0))
        case OUTPUT if io.consumers.isEmpty =>
          // if (Driver.saveConnectionWarnings) {
          //   ChiselError.warning("" + io + " UNCONNECTED IN " + io.component + " BINDING " + c.findBinding(io))
          // } removed this warning because pruneUnconnectedsIOs should have picked it up
          portDec insert (0, "//")
        case OUTPUT => c.parent.findBinding(io) match {
          case None => 
            if (Driver.saveConnectionWarnings) {
              ChiselError.warning("" + io + "(" + io.component + ") OUTPUT UNCONNECTED (" + 
                                  io.consumers.size + ") IN " + c.parent) }
            portDec insert (0, "//")
          case Some(consumer) => 
            if (io.prune) portDec insert (0, "//")
            portDec append emitRef(consumer)
        }
      }
      portDec append " )"
      portDecs += portDec
    }
    val uncommentedPorts = portDecs.filter(!_.result().contains("//"))
    uncommentedPorts.init map (_ append ",")
    portDecs map (_ insert (0, "       "))
    if (c.clocks.nonEmpty || c.resets.nonEmpty) res append ",\n" else res append "\n"
    res append (portDecs addString (new StringBuilder, "\n"))
    res append "\n  );\n"
    val driveRandPorts = c.wires filter (_._2.driveRand)
    if (!driveRandPorts.isEmpty) res append if_not_synthesis
    driveRandPorts map { case (n ,w) =>  
      List("    assign ", c.name, ".", n, " = ", emitRand(w), ";\n").mkString } addString res
    if (!driveRandPorts.isEmpty) res append endif_not_synthesis
    res
  }

  override def emitDef(node: Node): String = {
    val res = node match {
      case x: Bits if x.isIo && x.dir == INPUT => ""
      case x: Bits if node.inputs.isEmpty => 
        if (Driver.saveConnectionWarnings) ChiselError.warning("UNCONNECTED " + node + " IN " + node.component)
        List("  assign ", emitTmp(node), " = ", emitRand(node), ";\n").mkString
      case x: Bits =>
        List("  assign ", emitTmp(node), " = ", emitRef(node.inputs(0)), ";\n").mkString

      case x: Mux =>
        List("  assign ", emitTmp(x), " = ", emitRef(x.inputs(0)), " ? ", emitRef(x.inputs(1)), " : ", emitRef(x.inputs(2)), ";\n").mkString

      case o: Op if o.op == "##" => 
        List("  assign ", emitTmp(o), " = ", "{", emitRef(node.inputs(0)), ", ", emitRef(node.inputs(1)), "}", ";\n").mkString
      case o: Op if o.inputs.size == 1 => 
        List("  assign ", emitTmp(o), " = ", o.op, " ", emitRef(node.inputs(0)), ";\n").mkString
      case o: Op if o.op == "s*s" || o.op == "s*u" || o.op == "s%s" || o.op == "s/s" => 
        List("  assign ", emitTmp(o), " = ", "$signed(", emitRef(node.inputs(0)), ") ", o.op(1), " $signed(", emitRef(node.inputs(1)), ")", ";\n").mkString
      case o: Op if o.op == "s<" || o.op == "s<=" => 
        List("  assign ", emitTmp(o), " = ", "$signed(", emitRef(node.inputs(0)), ") ", o.op.tail, " $signed(", emitRef(node.inputs(1)), ")", ";\n").mkString
      case o: Op if o.op == "s>>" => 
        List("  assign ", emitTmp(o), " = ", "$signed(", emitRef(node.inputs(0)), ") >>> ", emitRef(node.inputs(1)), ";\n").mkString
      case o: Op => 
        List("  assign ", emitTmp(o), " = ", emitRef(node.inputs(0)), " ", o.op, " ", emitRef(node.inputs(1)), ";\n").mkString

      case x: Extract =>
        node.inputs.tail.foreach(x.validateIndex)
        val gotWidth = node.inputs(0).needWidth()
        List("  assign " + emitTmp(node) + " = " + emitRef(node.inputs(0)),
          if (node.inputs.size < 3 && gotWidth > 1) List("[", emitRef(node.inputs(1)), "]").mkString 
          else if (gotWidth > 1) List("[", emitRef(node.inputs(1)) , ":", emitRef(node.inputs(2)), "]").mkString
          else "",
        ";\n").mkString

      case m: Mem[_] if !m.isInline => 
        def gcd(a: Int, b: Int) : Int = { if(b == 0) a else gcd(b, a%b) }
        def find_gran(x: Node) : Int = x match {
          case _: Literal => x.needWidth()
          case _: UInt => if (x.inputs.isEmpty) 1 else find_gran(x.inputs(0))
          case _: Mux => gcd(find_gran(x.inputs(1)), find_gran(x.inputs(2)))
          case _: Op => x.inputs.map(find_gran).max
          case _ => 1
        }
        val mask_writers = m.writeAccesses.filter(_.isMasked)
        val mask_grans = mask_writers.map(x => find_gran(x.mask))
        val mask_gran = if (mask_grans.nonEmpty && mask_grans.forall(_ == mask_grans(0))) mask_grans(0) else 1
        val configStr = List(
         " depth ", m.n,
         " width ", m.needWidth(),
         " ports ", m.ports map (_.getPortType) mkString ",",
         if (mask_gran != 1) " mask_gran " + mask_gran else "", "\n").mkString
        val name = getMemName(m, configStr)
        ChiselError.info("MEM " + name)

        val clk = List("    .CLK(", emitRef(m.clock.get), ")").mkString
        val portdefs = m.ports.zipWithIndex map { case (p, i) => emitPortDef(p, i) }
        List("  ", name, " ", emitRef(m), " (\n", (clk +: portdefs) mkString ",\n", "\n", "  );\n").mkString

      case m: MemRead if m.mem.isInline =>
        List("  assign ", emitTmp(node), " = ", emitRef(m.mem), "[", emitRef(m.addr), "];\n").mkString

      case r: ROMRead =>
        val inits = r.rom.sparseLits map { case (i, v) => 
          s"    ${i}: ${emitRef(r)} = ${emitRef(v)};\n" } addString new StringBuilder
        (List(s"  always @(*) case (${emitRef(r.inputs.head)})\n",
        inits.result,
        s"    default: begin\n",
        s"      ${emitRef(r)} = ${r.needWidth()}'bx;\n",
        if_not_synthesis,
        s"      ${emitRef(r)} = ${emitRand(r)};\n",
        endif_not_synthesis,
        s"    end\n",
        "  endcase\n") addString new StringBuilder).result()

      case s: Sprintf =>
        List("  always @(*) $sformat(", emitTmp(s), ", ", (CString(s.format) +: (s.args map emitRef)) mkString ", ", ");\n").mkString

      case _ =>
        ""
    }
    List(if (node.prune && res != "") "//" else "", res).mkString
  }

  def emitDecBase(node: Node, wire: String = "wire"): String =
    s"  ${wire}${emitWidth(node)} ${emitRef(node)};\n"

  def emitDecReg(node: Node): String = emitDecBase(node, "reg ")

  override def emitDec(node: Node): String = {
    val gotWidth = node.needWidth()
    val res =
    node match {
      case x: Bits if x.isIo => ""

      case _: Assert =>
        List("  reg", "[", gotWidth-1, ":0] ", emitRef(node), ";\n").mkString

      case _: Reg =>
        emitDecReg(node)

      case _: Sprintf =>
        emitDecReg(node)

      case _: ROMRead =>
        emitDecReg(node)

      case m: Mem[_] if !m.isInline => ""
      case m: Mem[_] => 
        List("  reg [", m.needWidth()-1, ":0] ", emitRef(m), " [", m.n-1, ":0];\n").mkString

      case x: MemAccess =>
        x.referenced = true
        emitDecBase(node)

      case _: ROMData => ""

      case _: Literal => ""

      case _ =>
        emitDecBase(node)
    }
    List(if (node.prune && res != "") "//" else "", res).mkString
  }

  def emitInit(node: Node): String = node match {
    case r: Reg =>
      List("    ", emitRef(r), " = ", emitRand(r), ";\n").mkString
    case m: Mem[_] if m.isInline => 
      "    for (initvar = 0; initvar < " + m.n + "; initvar = initvar+1)\n" +
      "      " + emitRef(m) + "[initvar] = " + emitRand(m) + ";\n"
    case a: Assert =>
      "    " + emitRef(a) + " = 1'b0;\n"
    case _ =>
      ""
  }

  // Is the specified node synthesizeable?
  // This could be expanded. For the moment, we're flagging unconnected Bits,
  // for which we generate un-synthesizable random values.
  def synthesizeable(node: Node): Boolean = {
    node match {
      case x: Bits =>
        if (x.isIo && x.dir == INPUT) {
          true
        } else if (node.inputs.nonEmpty) {
          true
        } else {
          false
        }
      case _ => true
    }
  }

  def emitDefs(c: Module): StringBuilder = {
    val resSimulate = new StringBuilder()
    val resSynthesis = new StringBuilder()
    val res = new StringBuilder()
    for (m <- c.nodes) {
      val resNode = if (synthesizeable(m)) {
        resSynthesis
      } else {
        resSimulate
      }
      resNode.append(emitDef(m))
    }
    // Did we generate any non-synthesizable definitions?
    if (resSimulate.nonEmpty) {
      res.append(if_not_synthesis)
      res ++= resSimulate
      res.append(endif_not_synthesis)
    }
    res ++= resSynthesis
    for (c <- c.children) {
      res.append(emitDef(c))
    }
    res
  }

  def emitRegs(c: Module): StringBuilder = {
    val res = new StringBuilder
    val clkDomains = (c.clocks map (_ -> new StringBuilder)).toMap
    if (Driver.isAssert) {
      c.asserts foreach (p => p.clock match {
        case Some(clk) if clkDomains contains clk =>
          clkDomains(clk) append emitAssert(p)
        case _ =>
      })
    }
    c.nodes foreach (m => m.clock match {
      case Some(clk) if clkDomains contains clk =>
        clkDomains(clk) append emitReg(m)
      case _ => 
    })
    c.printfs foreach (p => p.clock match {
      case Some(clk) if clkDomains contains clk => 
        clkDomains(clk) append emitPrintf(p)
      case _ =>
    })
    for ((clock, dom) <- clkDomains ; if dom.nonEmpty) {
      if (res.isEmpty) res.append("\n")
      res.append("  always @(posedge " + emitRef(clock) + ") begin\n")
      res.append(dom)
      res.append("  end\n")
    }
    res
  }

  def emitPrintf(p: Printf): String = {
    val file = if (Driver.isGenHarness) "32'h80000001" else "32'h80000002"
    (List(if_not_synthesis,
    "`ifdef PRINTF_COND\n",
    "    if (`PRINTF_COND)\n",
    "`endif\n",
    s"      fd = $file;\n",
    "      if (", emitRef(p.cond), ")\n",
    "        $fwrite(", "fd", ", ", (CString(p.format) +: (p.args map emitRef)) mkString ", ", ");\n",
    endif_not_synthesis) addString new StringBuilder).result()
  }
  def emitAssert(a: Assert): String = {
    val file = if (Driver.isGenHarness) "32'h80000001" else "32'h80000002"
    (List(if_not_synthesis,
      s"      fd = $file;\n",
    "  if(", emitRef(a.reset), ") ", emitRef(a), " <= 1'b1;\n",
    "  if(!", emitRef(a.cond), " && ", emitRef(a), " && !", emitRef(a.reset), ") begin\n",
    "    $fwrite(", "fd", ", ", CString("ASSERTION FAILED: %s\n"), ", ", CString(a.message), ");\n",
    "    $finish;\n",
    "  end\n",
    endif_not_synthesis) addString new StringBuilder).result()
  }

  def emitReg(node: Node): String = {
    node match {
      case reg: Reg =>
        def cond(c: Node) = List("if(", emitRef(c), ") begin").mkString
        def uncond = "begin"
        def sep = "\n      "
        def assign(r: Reg, x: Node) = List(emitRef(r), " <= ", emitRef(x), ";\n").mkString
        def traverseMuxes(r: Reg, x: Node): List[String] = x match {
          case m: Mux => (cond(m.inputs(0)) + sep + assign(r, m.inputs(1))) :: traverseMuxes(r, m.inputs(2))
          case _ => if (x eq r) Nil else List(uncond + sep + assign(r, x))
        }
        reg.next match { 
          case _: Mux =>  
            List("    ", traverseMuxes(reg, reg.next) mkString "    end else ", "    end\n").mkString
          case _ => 
            List("    ", assign(reg, reg.next)).mkString
        }

      case m: MemWrite if m.mem.isInline => List(
        "    if (", emitRef(m.cond), ")\n",
        "      ", emitRef(m.mem), "[", emitRef(m.addr), "] <= ", emitRef(m.data), ";\n").mkString

      case _ =>
        ""
    }
  }

  def emitDecs(c: Module): StringBuilder =
    c.nodes.map(emitDec).addString(new StringBuilder)

  def emitInits(c: Module): StringBuilder = {
    val sb = new StringBuilder
    c.nodes.map(emitInit).addString(sb)

    val res = new StringBuilder
    if (sb.nonEmpty) {
      res append if_not_synthesis
      res append "  integer fd;\n"
      res append "  integer initvar;\n"
      res append "  initial begin\n"
      res append "    #0.002;\n"
      res append sb
      res append "  end\n"
      res append endif_not_synthesis
    }
    res
  }

  def emitModuleText(c: Module): StringBuilder = c match {
    case _: BlackBox => new StringBuilder 
    case _ =>

    val res = new StringBuilder()
    var first = true
    (c.clocks ++ c.resets.values.toList) map ("input " + emitRef(_)) addString (res, ", ")
    val ports = mutable.ArrayBuffer[StringBuilder]()
    for ((n, io) <- c.wires) {
      val prune = if (io.prune && c != topMod) "//" else ""
      io.dir match {
        case INPUT =>
          ports += List("    ", prune, "input ", emitWidth(io), " ", emitRef(io)) addString new StringBuilder
        case OUTPUT =>
          ports += List("    ", prune, "output", emitWidth(io), " ", emitRef(io)) addString new StringBuilder
      }
    }
    val uncommentedPorts = ports.filter(!_.result().contains("//"))
    uncommentedPorts.init map (_ append ",")
    if (c.clocks.nonEmpty || c.resets.nonEmpty) res.append(",\n") else res.append("\n")
    res.append(ports addString (new StringBuilder, "\n"))
    res.append("\n);\n\n")
    res.append(emitDecs(c))
    res.append("\n")
    res.append(emitInits(c))
    res.append("\n")
    res.append(emitDefs(c))
    res.append(emitRegs(c))
    res.append("endmodule\n\n")
    res
  }

  def flushModules(
    defs: mutable.LinkedHashMap[String, mutable.LinkedHashMap[StringBuilder, mutable.ArrayBuffer[Module]]],
    level: Int ): Unit =
  {
    for( (className, modules) <- defs ) {
      var index = 0
      for ( (text, comps) <- modules) {
        val moduleName = if( modules.nonEmpty ) {
          className + "_" + index.toString
        } else {
          className
        }
        index = index + 1
        for( flushComp <- comps ) {
          if( flushComp.level == level && flushComp.moduleName == "") {
            flushComp.moduleName = moduleName
          }
        }
      /* XXX We write the module source text in *emitChildren* instead
             of here so as to generate a minimal "diff -u" with the previous
             implementation. */
      }
    }
  }


  def emitChildren(top: Module,
    defs: mutable.LinkedHashMap[String, mutable.LinkedHashMap[StringBuilder, mutable.ArrayBuffer[Module] ]],
    out: java.io.FileWriter, depth: Int): Unit = top match {
    case _: BlackBox =>
    case _ =>

    // First, emit my children
    for (child <- top.children) {
      emitChildren(child, defs, out, depth + 1)
    }

    // Now, find and emit me
    // Note: emittedModules used to ensure modules only emitted once
    //    regardless of how many times used (e.g. when folded)
    val className = extractClassName(top)
    for{
      (text, comps) <- defs(className)
      if comps contains top
      if !(emittedModules contains top.moduleName)
    } {
      out.append(s"module ${top.moduleName}(")
      out.append(text)
      emittedModules += top.moduleName
      return
    }
  }


  def doCompile(top: Module, out: java.io.FileWriter, depth: Int): Unit = {
    /* *defs* maps Mod classes to Mod instances through
       the generated text of their module.
       We use a LinkedHashMap such that later iteration is predictable. */
    val defs = mutable.LinkedHashMap[String, mutable.LinkedHashMap[StringBuilder, mutable.ArrayBuffer[Module]]]()
    var level = 0
    for (c <- Driver.sortedComps) {
      ChiselError.info(genIndent(depth) + "COMPILING " + c
        + " " + c.children.size + " CHILDREN"
        + " (" + c.level + "," + c.traversal + ")")
      ChiselError.checkpoint()

      if( c.level > level ) {
        /* When a component instance instantiates different sets
         of sub-components based on its constructor parameters, the same
         Module class might appear with different level in the tree.
         We thus wait until the very end to generate module names.
         If that were not the case, we could flush modules as soon as
         the source text for all components at a certain level in the tree
         has been generated. */
        flushModules(defs, level)
        level = c.level
      }
      val res = emitModuleText(c)
      val className = extractClassName(c)
      if( !(defs contains className) ) {
        defs += (className -> mutable.LinkedHashMap[StringBuilder, mutable.ArrayBuffer[Module] ]())
      }
      if( defs(className) contains res ) {
        /* We have already outputed the exact same source text */
        defs(className)(res) += c
        ChiselError.info("\t" + defs(className)(res).length + " components")
      } else {
        defs(className) += (res -> mutable.ArrayBuffer[Module](c))
      }
    }
    flushModules(defs, level)
    emitChildren(top, defs, out, depth)
  }

  override def elaborate(c: Module) {
    super.elaborate(c)
    // execute addBindings only in the Verilog Backend
    addBindings
    nameBindings
    findConsumers(c)

    val n = Driver.appendString(Some(c.name), Driver.chiselConfigClassName)
    val out = createOutputFile(n + ".v")
    doCompile(c, out, 0)
    ChiselError.checkpoint()
    out.close()

    def portsMap(io: Bits) = Map(
      "name" -> emitRef(io),
      "width" -> io.needWidth(),
      "msb" -> (io.needWidth() - 1) )

    val io = c.wires.map(_._2).toSeq groupBy (_.dir)

    type StringMap = Map[String, Any]
    def mkPortList[T](l: Seq[T], portsMap: T => StringMap = identity[StringMap] _): Seq[StringMap] =
      l.init.map(portsMap) :+ (portsMap(l.last) + ("last" -> true))

    object templData {
      val m = Map("moduleName" -> c.moduleName, "name" -> c.name)
      val ins = mkPortList(io(INPUT), portsMap)
      val outs = mkPortList(io(OUTPUT), portsMap)
      val mainClk = Driver.implicitClock.name
      val clocks = mkPortList(Driver.clocks.map(clk => Map("name" -> clk.name, "period" -> (
        clk.srcClock match {
          case None => "`CLOCK_PERIOD"
          case Some(src) => s"${src.name}_len ${if (src.period > clk.period)
              " / " + (src.period / clk.period).round else
              " * " + (clk.period / src.period).round}"
        })
      )))
      val driver = Map("targetDir" -> Driver.targetDir) ++
        (if(Driver.isVCD) Map("vcd" -> simulator.vcdHarness(c.moduleName, Driver.isVCDMem)) else Map())
      val resets = mkPortList(c.resets.values.toSeq.map(r => Map("name" -> r.name)))
    }

    if (memConfs.nonEmpty) {
      val out_conf = createOutputFile(n + ".conf")
      out_conf.write(getMemConfString)
      out_conf.close()
    }

    if (Driver.isGenHarness) {
      val template =
        simulatorName match {
          case "verilator" =>
            copyToTarget("vl.h")
            "/CppHarness.handlebars.cpp"
          case _ =>
            copyToTarget("vpi.h")
            copyToTarget("vpi.cpp")
            "/VerilogHarness.handlebars.v"
        }

      val t = Handlebars(scala.io.Source.fromURL(getClass.getResource(template)).mkString)
      val harness = createOutputFile(n + "-harness" + template.substring(template.lastIndexOf('.')))
      harness write t(templData)
      harness.close()
      copyToTarget("sim_api.h")
    }

  }

  override def compile(c: Module, flags: Option[String]) {
    val n = Driver.appendString(Some(c.name), Driver.chiselConfigClassName)
    simulator.compile(c, n, flags)
  }

  private def if_not_synthesis = "`ifndef SYNTHESIS\n// synthesis translate_off\n"
  private def endif_not_synthesis = "// synthesis translate_on\n`endif\n"
}