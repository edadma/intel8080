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

  protected def ni: Nothing = sys.error("not implemented")

  def disassemble(cpu: CPU): (String, Int)

}

class MOV(d: Int, s: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.writeReg(d, cpu.readReg(s))
    1
  }

  def disassemble(cpu: CPU): (String, Int) = (s"MOV ${reg(d)}, ${reg(s)}", 1)

}

class MVI(d: Int, s: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.writeReg(d, cpu.readReg(s))
    1
  }

  def disassemble(cpu: CPU): (String, Int) = (s"MOV ${reg(d)}, ${reg(s)}", 1)

}

class RST(n: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    if (!cpu.trap(n))
      ni

    1
  }

  def disassemble(cpu: CPU): (String, Int) = (s"RST $n", 1)

}

object ILLEGAL extends Instruction {

  def apply(cpu: CPU): Int = {
    sys.error("illegal instruction")
  }

  def disassemble(cpu: CPU): (String, Int) = ("ILLEGAL", 1)

}
