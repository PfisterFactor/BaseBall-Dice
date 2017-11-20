import java.awt.GraphicsEnvironment

import Base.Base

import scala.util.Random

/**
  * Created by ej on 1/23/17.
  */

// So all this code is thrown together quite sloppily
// But I got it done in one day
// So thats a win in my book
object Main extends App {
  // Our random object that controls all the random dice rolls
  val randomObject: Random = new Random()

  // Players
  var Player1: Player = null
  var Player2: Player = null

  // Rolls a ten sided die
  // Returns: A psuedo-random integer between 0 to 9 inclusive
  def rollDice(): Int = randomObject.nextInt(10)

  def waitForInput[T](validInputs: Map[String, T], errorString: String = "Invalid input!"): T = {
    var read = scala.io.StdIn.readLine().toLowerCase
    while (!validInputs.contains(read)) {
      println(errorString)
      read = scala.io.StdIn.readLine().toLowerCase
    }
    validInputs(read)
  }

  val console = System.console()
  if(console == null && !GraphicsEnvironment.isHeadless()){
    val filename = this.getClass.getProtectionDomain().getCodeSource().getLocation().toString().substring(6)
    Runtime.getRuntime().exec(Array[String]("cmd","/c","start","cmd","/k","JRE\\bin\\java.exe -jar \"" + filename + "\""))
  }else{
    run(Array())
    System.out.println("Program has ended, please type 'exit' to close the console");
  }

  def run(args:Array[String]) = {
    // Introduction
    println("Hello!")
    println("Welcome to BaseballDice!")
    println("A Quality Nico Piscopo Game")
    println("First, what is Player one's name?")
    val playerOneName = scala.io.StdIn.readLine().capitalize
    println(s"Player one is now known as $playerOneName.")
    println("Second, what is Player two's name?")
    val playerTwoName = scala.io.StdIn.readLine().capitalize
    println(s"Player two is now known as $playerTwoName.")

    val player1PreGameRoll = PreGameRolls()
    val player2PreGameRoll = PreGameRolls()

    Player1 = Player(player1PreGameRoll, playerOneName)
    Player2 = Player(player2PreGameRoll, playerTwoName)

    println(s"Now ${Player1.Name} is gonna roll for bonus points.")
    println(s"Press enter to roll.")
    scala.io.StdIn.readLine()
    println(s"${Player1.Name} has rolled a ${Player1.PregameRolls.RollBonusPoints}. ${Player1.Name} gets ${Player1.PregameRolls.BonusPoints} bonus hitter points.\n")

    println(s"Now ${Player2.Name} is gonna roll for bonus points.")
    println(s"Press enter to roll.")
    scala.io.StdIn.readLine()
    println(s"${Player2.Name} has rolled a ${Player2.PregameRolls.RollBonusPoints}. ${Player2.Name} gets ${Player2.PregameRolls.BonusPoints} bonus hitter points.\n")

    println(s"Next, ${Player1.Name} is gonna roll for pinch hitter quality.")
    println(s"Press enter to roll.")
    scala.io.StdIn.readLine()
    println(s"${Player1.Name} has rolled a ${Player1.PregameRolls.RollPinchHitter}. ${Player1.Name} gets a ${Player1.PregameRolls.PinchHitterQuality} P/C pinch hitter.\n")

    println(s"Next, ${Player2.Name} is gonna roll for pinch hitter quality.")
    println(s"Press enter to roll.")
    scala.io.StdIn.readLine()
    println(s"${Player2.Name} has rolled a ${Player2.PregameRolls.RollPinchHitter}. ${Player2.Name} gets a ${Player2.PregameRolls.PinchHitterQuality} P/C pinch hitter.\n")

    println(s"Next, ${Player1.Name} is gonna roll for defense quality.")
    println(s"Press enter to roll.")
    scala.io.StdIn.readLine()
    println(s"${Player1.Name} has rolled a ${Player1.PregameRolls.RollDefenseQuality}. ${Player1.Name} gets a defense stat of ${Player1.PregameRolls.DefensePercent}%\n")

    println(s"Next, ${Player2.Name} is gonna roll for defense quality.")
    println(s"Press enter to roll.")
    scala.io.StdIn.readLine()
    println(s"${Player2.Name} has rolled a ${Player2.PregameRolls.RollDefenseQuality}. ${Player2.Name} gets a defense stat of ${Player2.PregameRolls.DefensePercent}%\n")

    println(s"Now ${Player1.Name} is gonna decide their team lineup.")
    Player1.assignPlayerPoints()
    println()
    println(s"Now ${Player2.Name} is gonna decide their team lineup.")
    Player2.assignPlayerPoints()

    println("The team lineups are decided. Its time to play ball!")

    Array("FIRST","SECOND","THIRD","FOURTH","FIFTH","SIXTH","SEVENTH","EIGHTH","NINTH").foreach(inning => {
      print("\u001b[2J")
      println(s"\n\n----$inning INNING----")
      doInningLoop()
    })
    print("\u001b[2J")
    println("Final score:")
    println(s"${Player1.Name} -- ${Player1.Points}")
    println(s"${Player2.Name} -- ${Player2.Points}")
    val winner = if (Player1.Points > Player2.Points) Player1 else if (Player2.Points > Player1.Points) Player2 else null

    if (winner != null)
      println(s"${winner.Name} is the winner!")
    else
      println("It's a tie!")

    println("Thanks for playing BaseballDice!")
    println("Goodbye!")
    Hitter.clearNames()
  }






  def doInningLoop() = {
    def hitting(hitting:Player,defending:Player) = {
      var hitterIndex: Int = 0
      while (hitting.Outs < 3 && hitterIndex < 9) {
        var hitter = hitting.Hitters(hitterIndex)
        println(s"${hitting.Outs} out(s)!")
        println(s"${hitter.Points}pt ${hitter.Type} hitter ${hitter.Name} is up to bat!")
        if (!hitting.PinchHitterUsed) {
          println(s"There is the option to substitute in your ${hitting.PregameRolls.PinchHitterQuality}pt pinch hitter.")
          println("Type \"pinch\" to sub them in or press enter otherwise")
          val subInHitter = waitForInput(Map(("pinch",true),("",false)))
          if (subInHitter) {
            println("Use them as a contact or power hitter?")
            val hitterType = waitForInput(Map(("contact",HitterType.Contact),("power",HitterType.Power)))
            val oldHitter = hitter
            if (hitterType == HitterType.Power)
              hitter = hitting.PinchPowerHitter
            else if (hitterType == HitterType.Contact)
              hitter = hitting.PinchContactHitter
            println(s"${oldHitter.Points}pt ${oldHitter.Type} hitter ${oldHitter.Name} is being switched out for ${hitter.Points}pt pinch ${hitter.Type} hitter ${hitter.Name}")
            hitting.PinchHitterUsed = true
          }
        }


        // To be implemented upon clarification of the rule
        /*
        if (hitting.Outs < 2 && hitting.Hitters.exists(_.getBase() == Base.Third)) {
          println(s"${hitting.Name} has the option of trying to send his")
        }
        */
        var strikeCount = 1
        var base = Base.None
        while (strikeCount <= 3 && base == Base.None) {
          base = doHittingDiceRoll(hitter, strikeCount)
          if (base == Base.None)
            strikeCount += 1
          else if (base == Base.First && hitting.Hitters.exists(_.getBase() == Base.Second)) {
            val secondbaseman = hitting.Hitters.find(_.getBase() == Base.Second).get
            println(s"${hitting.Name} has the option of trying to send his second base runner ${secondbaseman.Name} home!")
            println(s"Does ${hitting.Name} try it? Yes or no.")
            val choice = waitForInput(Map(("yes",true),("no",false)))
            if (choice) {
              println("Press enter to roll dice...\n")
              scala.io.StdIn.readLine()
              val diceRoll1 = rollDice()
              println(diceRoll1 + "!")
              scala.io.StdIn.readLine()
              val diceRoll2 = rollDice()
              println(diceRoll2 + "!")
              println(s"\n${hitter.Name} rolled a ${diceRoll1 + diceRoll2}")
              if (Set(6,7,12,13).contains(diceRoll1+diceRoll2)) {
                println(s"Second base runner ${secondbaseman.Name} was forced out!")
                hitting.Outs += 1
              }
              else {
                println(s"Second base runner ${secondbaseman.Name} made it home!")
                secondbaseman.setBase(Base.Home)
                hitting.Points += 1
                println(s"${hitting.Name} has ${hitting.Points} point(s)!")
              }
            }
          }
        }
        if (strikeCount > 3) {
          println(s"${hitter.Name} is out!")
          hitting.Outs += 1
        }
        else {
          println(s"${defending.Name} now has to roll to defend!")
          doDefenseDiceRoll(defending, hitting, hitter, base)
        }
        hitterIndex += 1
      }
      if (hitting.Outs >= 3) {
        println(s"3 Outs! ${hitting.Name} is no longer batting!")
      }
      else if (hitterIndex >= 10) {
        println(s"All of ${hitting.Name}'s hitters have batted! ${hitting.Name} is no longer batting!")
      }
      hitting.Outs = 0
      hitting.Hitters.foreach(_.setBase(Base.None))
      hitting.PinchHitterUsed = false
      println("Press enter to continue...")
      scala.io.StdIn.readLine()

    }
    println(s"${Player1.Name} is batting first!\n")
    println(s"${Player2.Name} is on defense.\n")
    hitting(Player1,Player2)
    println()
    println(s"${Player2.Name} is now batting\n")
    println(s"${Player1.Name} is on defense.\n")
    hitting(Player2,Player1)
    println()
  }

  def doHittingDiceRoll(hitter: Hitter, strike: Int): Base = {
    println("Press enter to roll dice...\n")
    scala.io.StdIn.readLine()
    val diceRoll1 = rollDice()
    println(diceRoll1 + "!")
    scala.io.StdIn.readLine()
    val diceRoll2 = rollDice()
    println(diceRoll2 + "!")
    println(s"\n${hitter.Name} rolled a ${diceRoll1 + diceRoll2}")
    val base = HitterSheet.getBaseFromRoll(diceRoll1 + diceRoll2, hitter)

    if (base == Base.None) {
      println(s"Strike $strike!")
      Base.None
    }
    else {
      println(s"A hit! ${hitter.Name} starts running! It's looking to be a ${Base.getName(base)}!")
      base
    }
  }
  def doDefenseDiceRoll(defender: Player,hitting:Player, hitter: Hitter, base: Base): Boolean = {
    println("Press enter to roll...")
    scala.io.StdIn.readLine()
    val diceRoll1 = rollDice()
    println(diceRoll1 + "!")
    scala.io.StdIn.readLine()
    val diceRoll2 = rollDice()
    print(diceRoll2 + "!")
    println(s"\nDefender rolled a ${diceRoll1 + diceRoll2}")
    val defenseStat = defender.PregameRolls.DefenseQuality
    if (defenseStat.contains(diceRoll1 + diceRoll2)) {
      if (base == Base.Home)
        println(s"The fielder robbed the home run! ${hitter.Type} hitter ${hitter.Name} is out!")
      else
        println(s"The fielder caught the ball! ${hitter.Type} hitter ${hitter.Name} is out!")
      hitting.Outs += 1
      true
    }
    else if (base == Base.Home && diceRoll1+diceRoll2 != 9) {
      println("The ball is flying high! It looks like it'll be a home run!")
      println("Press enter to roll...")
      scala.io.StdIn.readLine()
      val diceRoll1Again = rollDice()
      println(diceRoll1Again + "!")
      scala.io.StdIn.readLine()
      val diceRoll2Again = rollDice()
      println(diceRoll2Again + "!")
      println(s"\nDefender rolled a ${diceRoll1Again + diceRoll2Again}")
      if (diceRoll1Again + diceRoll2Again == 9) {
        println("A gust of wind blows the ball foul! The home run was denied!")
        hitting.Outs += 1
        true
      }
      else {
        println("The ball sails straight out of the field! A home run!")
        println(s"${hitter.Name} runs home!")
        hitter.setBase(Base.Home)
        hitting.Points += 1
        println(s"${hitting.Name} has ${hitting.Points} point(s)!")
        Hitter.advanceHitters(hitting,hitter,base)
        false
      }
    }
    else {
      println(s"The ball sails through the air!")
      println(s"${hitter.Type} hitter ${hitter.Name} runs to ${base.toString.toLowerCase()} base before the defense can get the ball!")
      hitter.setBase(base)
      Hitter.advanceHitters(hitting, hitter, base)
      false
    }
  }
}
