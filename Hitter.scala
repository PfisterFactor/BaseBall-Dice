import Base.Base
import HitterType.HitterType

import scala.collection.mutable
import scala.util.Random

/**
  * Created by ej on 1/23/17.
  */
object Base extends Enumeration {
  type Base = Value
  val First = Value("First")
  val Second = Value("Second")
  val Third = Value("Third")
  val Home = Value("Home")
  val None = Value("None")

  def getNumber(base: Base) = base match {
    case First => 1
    case Second => 2
    case Third => 3
    case Home => 4
    case None => -1
  }
  def getBase(i:Int) = i match {
    case 1 => First
    case 2 => Second
    case 3 => Third
    case 4 => Home
    case _ => None
  }
  def getName(base:Base) = base match {
    case First => "Single"
    case Second => "Double"
    case Third => "Triple"
    case Home => "Home run"
    case _ => "Undefined"
  }
}
object HitterType extends Enumeration {
  type HitterType = Value
  val Power = Value("Power")
  val Contact = Value("Contact")
  val UNDEFINED = Value("Undefined")
}

object Hitter {
  final val FirstNames = Array(
    "Kyle",
    "Nico",
    "Patrick",
    "Pat",
    "Danny",
    "Daniel",
    "Joe",
    "Joey",
    "Gino",
    "Matt",
    "Matthew",
    "Bobby",
    "Tim",
    "Jerry"
  )

  final val LastNames = Array(
    "Pfister",
    "Piscopo",
    "Eder",
    "Murray",
    "Majerus",
    "Evans",
    "Gonzales",
    "Rogers",
    "Johnson",
    "Matthews",
    "Hercules",
    "Trashcan",
    "Booker",
    "Timothy"
  )

  val UsedNames:scala.collection.mutable.Set[String] = mutable.Set()

  def clearNames() = UsedNames.clear()

  def generateName():String = {
    if (UsedNames.size >= FirstNames.length * LastNames.length)
      return "OutOfNames"
    var name = FirstNames(Random.nextInt(FirstNames.length)) + " " + LastNames(Random.nextInt(LastNames.length))
    while (UsedNames.contains(name)) {
      name = FirstNames(Random.nextInt(FirstNames.length)) + " " + LastNames(Random.nextInt(LastNames.length))
    }
    UsedNames.add(name)
    name
  }

  def advanceHitters(player: Player,hitBy: Hitter,base:Base) = {
    player.Hitters.foreach(hitter => {
      if (hitter.getBase() != Base.None && hitter.getBase() != Base.Home && hitter != hitBy) {
        val newBaseNumber = Base.getNumber(hitter.getBase()) + Base.getNumber(base)
        if (Base.getBase(newBaseNumber) == Base.None || Base.getBase(newBaseNumber) == Base.Home) {
          println(s"${hitter.Name} runs home!")
          player.Points += 1
          println(s"${player.Name} has ${player.Points} point(s)!")
          hitter.setBase(Base.Home)
        }
        else {
          val base = Base.getBase(newBaseNumber)
          println(s"${hitter.Name} runs to $base!")
          hitter.setBase(base)
        }
      }
    })
  }

}
case class Hitter(Type:HitterType,Points:Int, Name:String = Hitter.generateName()) {
  def this() = this(HitterType.UNDEFINED,1)
  private var currentBase: Base = Base.None

  if (Points > 5 || Points < 1) throw new IllegalStateException("Points for hitter must be between 1-5 inclusive!")

  def setBase(base:Base) = if (base != null) currentBase = base
  def getBase():Base = currentBase

}
