package xyz.hyperreal.intel8080

abstract class Instruction extends (CPU => Int) with Regs {

  protected def reg(r: Int): String =
    r match {
      case RA => "A"
      case RB => "B"
      case RC => "C"
      case RD => "D"
      case RE => "E"
      case RH => "H"
      case RL => "L"
      case M  => "M"
    }

  protected def regPair(r: Int): String =
    r match {
      case RBC => "BC"
      case RDE => "DE"
      case RHL => "HL"
      case RSP => "SP"
    }

  protected def ni: Nothing = sys.error("not implemented")

  def disassemble(cpu: CPU): (String, Int)

}

class MOV(d: Int, s: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.writeReg(d, cpu.readReg(s))

    if (d == M || s == M) 7
    else 5
  }

  def disassemble(cpu: CPU): (String, Int) = (s"MOV ${reg(d)}, ${reg(s)}", 1)

}

class MVI(d: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.writeReg(d, cpu.fetchByte)

    if (d == M) 10
    else 7
  }

  def disassemble(cpu: CPU): (String, Int) = (s"MVI ${reg(d)}, #${hexByte(cpu.fetchByte)}", 2)

}

class LXI(p: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.writePair(p, cpu.fetchWord)
    10
  }

  def disassemble(cpu: CPU): (String, Int) = (s"LXI ${regPair(p)}, #${hexWord(cpu.fetchWord)}", 3)

}

object LDA extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.R(RA) = cpu.memory.readByte(cpu.fetchWord)
    13
  }

  def disassemble(cpu: CPU): (String, Int) = (s"LDA ${hexWord(cpu.fetchWord)}", 3)

}

object STA extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.memory.writeByte(cpu.fetchWord, cpu.R(RA))
    13
  }

  def disassemble(cpu: CPU): (String, Int) = (s"STA ${hexWord(cpu.fetchWord)}", 3)

}

class RST(n: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    if (!cpu.trap(n))
      ni

    11
  }

  def disassemble(cpu: CPU): (String, Int) = (s"RST $n", 1)

}

object ILLEGAL extends Instruction {

  def apply(cpu: CPU): Int = {
    sys.error(s"illegal instruction: ${hexWord(cpu.PC)}")
  }

  def disassemble(cpu: CPU): (String, Int) = ("ILLEGAL", 1)

}
