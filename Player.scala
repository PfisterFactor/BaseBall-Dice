
/**
  * Created by ej on 1/23/17.
  */
case class PreGameRolls() {
  val RollBonusPoints = Main.rollDice()
  val RollPinchHitter = Main.rollDice()
  val RollDefenseQuality = Main.rollDice()

  val BonusPoints: Int = RollBonusPoints

  val PinchHitterQuality: Int = RollPinchHitter match {
    case 9 => 5
    case 8 | 7 => 4
    case 6 | 5 => 3
    case 4 | 3 => 2
    case 2 | 1 | 0 => 1
    case _ => -1
  }

  val DefenseQuality: Set[Int] = {
    val Base = Set(6,7,8,0)
    RollDefenseQuality match {
      case 9 => Base ++ Set(5,10,11)
      case 8 => Base ++ Set(5,10)
      case i if 5 to 7 contains i => Base ++ Set(5,4)
      case i if 1 to 4 contains i => Base ++ Set(5)
      case 0 => Base
      case _ => Set()
    }
  }
  def DefensePercent:Int = RollDefenseQuality match {
    case 9 => 47
    case 8 => 40
    case i if 5 to 7 contains i => 36
    case i if 1 to 4 contains i => 31
    case 0 => 25
  }
}

case class Player(PregameRolls: PreGameRolls,Name:String) {
  val MaxPlayerPoints: Int = 25 + PregameRolls.BonusPoints
  var Hitters: Array[Hitter] = (0 until 9).map(i => new Hitter()).toArray
  var Points: Int = 0
  var Outs:Int = 0
  val PinchContactHitter:Hitter = Hitter(HitterType.Contact,PregameRolls.PinchHitterQuality)
  val PinchPowerHitter:Hitter = Hitter(HitterType.Power,PregameRolls.PinchHitterQuality)
  var PinchHitterUsed = false

  def assignPlayerPoints() = {
    var currentPlayerPoints = MaxPlayerPoints
    var hitterCount = 0
    Hitters = Hitters.map(hitter => {
      println(s"Assign a role for ${hitter.Name}. Contact or Power? (${Hitters.length - 1 - hitterCount} players left)")
      val hitterType = Main.waitForInput(Map(("power", HitterType.Power), ("contact", HitterType.Contact)), "Invalid role! Contact or Power?")

      var points = 1
      if (currentPlayerPoints > 0) {
        println(s"Assign points for ${hitter.Name}, 1 through 5. ${currentPlayerPoints} points left. (${Hitters.length - 1 - hitterCount} players left)")
        points = Main.waitForInput(Map(("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5)), "Invalid points! Input a number from 1 to 5.")
        if (points > currentPlayerPoints) {
          println(s"Not enough points, assigning hitter ${currentPlayerPoints} points instead.")
          points = currentPlayerPoints
          currentPlayerPoints = 0
        }
        else
          currentPlayerPoints -= points
      }
      else {
        println(s"Out of points! Assigning ${hitter.Name} 1 point.")
      }
      println(s"Hitter ${hitter.Name} is a ${points} point ${hitterType} hitter.\n")
      hitterCount+=1
      Hitter(hitterType, points, hitter.Name)
    })
    if (currentPlayerPoints > 0) {
      println(s"${currentPlayerPoints} unused player points. Assigning the rest automatically...")
      while (currentPlayerPoints > 0) {
        val hitterIndex = Hitters.indexWhere(_.Points < 5)
        val hitter = Hitters(hitterIndex)
        Hitters.update(hitterIndex,Hitter(hitter.Type,hitter.Points+1,hitter.Name))
        currentPlayerPoints -= 1
      }
    }
  }
}
