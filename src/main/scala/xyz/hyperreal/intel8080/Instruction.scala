package xyz.hyperreal.intel8080

abstract class Instruction extends (CPU => Int) with Const {

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
      case RBC => "B"
      case RDE => "D"
      case RHL => "H"
      case RSP => "SP"
    }

  protected def cond(c: Int): String =
    c match {
      case CNZ => "NZ"
      case CZ  => "Z"
      case CNC => "NC"
      case CC  => "C"
      case CPO => "PO"
      case CPE => "PE"
      case CP  => "P"
      case CM  => "M"
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

object LHLA extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.writePair(RHL, cpu.memory.readWord(cpu.fetchWord))
    16
  }

  def disassemble(cpu: CPU): (String, Int) = (s"LHLA ${hexWord(cpu.fetchWord)}", 3)

}

object SHLA extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.memory.writeWord(cpu.fetchWord, cpu.readPair(RHL))
    16
  }

  def disassemble(cpu: CPU): (String, Int) = (s"SHLA ${hexWord(cpu.fetchWord)}", 3)

}

class LDAX(p: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.R(RA) = cpu.memory.readByte(cpu.readPair(p))
    7
  }

  def disassemble(cpu: CPU): (String, Int) = (s"LDAX ${regPair(p)}", 1)

}

class STAX(p: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.memory.writeByte(cpu.readPair(p), cpu.R(RA))
    7
  }

  def disassemble(cpu: CPU): (String, Int) = (s"STAX ${regPair(p)}", 1)

}

object XCHG extends Instruction {

  def apply(cpu: CPU): Int = {
    val B = cpu.R(RB)
    val C = cpu.R(RC)

    cpu.R(RB) = cpu.R(RD)
    cpu.R(RC) = cpu.R(RE)
    cpu.R(RD) = B
    cpu.R(RE) = C
    4
  }

  def disassemble(cpu: CPU): (String, Int) = ("XCHG", 1)

}

//

class INX(p: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.writePair(p, cpu.readPair(p) + 1)
    5
  }

  def disassemble(cpu: CPU): (String, Int) = (s"INX ${regPair(p)}", 1)

}

class DCX(p: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.writePair(p, cpu.readPair(p) - 1)
    5
  }

  def disassemble(cpu: CPU): (String, Int) = (s"DCX ${regPair(p)}", 1)

}

//

object CMA extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.R(RA) ^= 0xFF
    4
  }

  def disassemble(cpu: CPU): (String, Int) = ("CMA", 1)

}

object CMC extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.C ^= true
    4
  }

  def disassemble(cpu: CPU): (String, Int) = ("CMC", 1)

}

object STC extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.C = true
    4
  }

  def disassemble(cpu: CPU): (String, Int) = ("STC", 1)

}

object JMP extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.PC = cpu.fetchWord
    10
  }

  def disassemble(cpu: CPU): (String, Int) = (s"JMP ${hexWord(cpu.fetchWord)}", 3)

}

class Jccc(c: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    if (cpu.cond(c))
      cpu.PC = cpu.fetchWord

    10
  }

  def disassemble(cpu: CPU): (String, Int) = (s"J${cond(c)} ${hexWord(cpu.fetchWord)}", 3)

}

object CALL extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.push(cpu.PC)
    cpu.PC = cpu.fetchWord
    17
  }

  def disassemble(cpu: CPU): (String, Int) = (s"CALL ${hexWord(cpu.fetchWord)}", 3)

}

class Cccc(c: Int) extends Instruction {

  def apply(cpu: CPU): Int =
    if (cpu.cond(c)) {
      cpu.push(cpu.PC)
      cpu.PC = cpu.fetchWord
      17
    } else
      11

  def disassemble(cpu: CPU): (String, Int) = (s"C${cond(c)} ${hexWord(cpu.fetchWord)}", 3)

}

object RET extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.PC = cpu.pop
    10
  }

  def disassemble(cpu: CPU): (String, Int) = ("RET", 1)

}

class Rccc(c: Int) extends Instruction {

  def apply(cpu: CPU): Int =
    if (cpu.cond(c)) {
      cpu.PC = cpu.pop
      11
    } else
      5

  def disassemble(cpu: CPU): (String, Int) = (s"R${cond(c)} ${hexWord(cpu.fetchWord)}", 3)

}

class RST(n: Int) extends Instruction {

  def apply(cpu: CPU): Int = {
    if (!cpu.trap(n))
      ni

    11
  }

  def disassemble(cpu: CPU): (String, Int) = (s"RST $n", 1)

}

object PCHL extends Instruction {

  def apply(cpu: CPU): Int = {
    cpu.PC = cpu.readPair(RHL)
    5
  }

  def disassemble(cpu: CPU): (String, Int) = ("PCHL", 1)

}

object ILLEGAL extends Instruction {

  def apply(cpu: CPU): Int = {
    sys.error(s"illegal instruction: ${hexWord(cpu.PC)}")
  }

  def disassemble(cpu: CPU): (String, Int) = ("ILLEGAL", 1)

}
