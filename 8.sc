val input = "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10"

val InstPattern = """([\D]+) (inc|dec) (-?\d*) if (\D+) ([!<>=]+) (-?\d*)""".r
val insts = input.split("\\n").map {
  case InstPattern(register, cmd, value, condReg, condOp, condVal) =>
    Inst(register,
      delta = if (cmd == "inc") value.toInt else -value.toInt,
      cond = condOp match {
        case ">" => _.getOrElse(condReg, 0) > condVal.toInt
        case "<" => _.getOrElse(condReg, 0) < condVal.toInt
        case ">=" => _.getOrElse(condReg, 0) >= condVal.toInt
        case "<=" => _.getOrElse(condReg, 0) <= condVal.toInt
        case "==" => _.getOrElse(condReg, 0) == condVal.toInt
        case "!=" => _.getOrElse(condReg, 0) != condVal.toInt
      })
}

val MAX_REG = "__max__"
val res = (insts foldLeft Map[String, Int]()) { case (registers, Inst(register, delta, cond)) =>
              if (!cond(registers)) registers
              else {
                val newVal = registers.getOrElse(register, 0) + delta
                registers
                  .updated(register, newVal)
                  .updated(MAX_REG, math.max(registers.getOrElse(MAX_REG, 0), newVal))
              }
            }

val pt1 = res.filterKeys(_ != MAX_REG).values.max
val pt2 = res.getOrElse(MAX_REG, 0)

case class Inst(register: String, delta: Int, cond: (Map[String, Int]) => Boolean)
