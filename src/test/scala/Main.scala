package xyz.hyperreal.intel8080

object Main extends App {

  val mem =
    new Memory {
      def init: Unit = {
        removeDevices
        regions.clear
        add(new ROM("program", 0, 0x7FFF))
        add(new RAM("ram", 0x1000, 0xFFFF))
      }
    }
  val cpu = new CPUWithServices { memory = mem }

  Hex(mem,
      """
      |0000:
      |  
      |""".stripMargin)

}
