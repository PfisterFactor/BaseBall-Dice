import Base.Base

/**
  * Created by ej on 1/23/17.
  */
object HitterSheet {
  // The award for the most nested collection...
  val PowerHitterSheet:Array[Map[Int,Base]] = Array(
    Map(
      (15,Base.First),
      (1,Base.Second),
      (1,Base.Third),
      (5,Base.Home)
    ),
    Map(
      (6,Base.First),
      (16,Base.Second),
      (17,Base.Third),
      (7,Base.Home)
    ),
    Map(
      (5,Base.First),
      (15,Base.Second),
      (16,Base.Third),
      (13,Base.Home),
      (14,Base.Home)
    ),
    Map(
      (6,Base.First),
      (4,Base.Second),
      (18,Base.Third),
      (9,Base.Home),
      (12,Base.Home)
    ),
    Map(
      (9,Base.First),
      (15,Base.Second),
      (18,Base.Third),
      (6,Base.Home),
      (12,Base.Home),
      (13,Base.Home)
    )
  )

  val ContactHitterSheet:Array[Map[Int,Base]] = Array(
    Map(
      (9,Base.First),
      (14,Base.Second),
      (17,Base.Third),
      (18,Base.Home)
    ),
    Map(
      (1,Base.First),
      (9,Base.First),
      (4,Base.Second),
      (17,Base.Third),
      (16,Base.Home)
    ),
    Map(
      (4,Base.First),
      (13,Base.First),
      (14,Base.First),
      (5,Base.Second),
      (17,Base.Third),
      (3,Base.Home)
    ),
    Map(
      (9,Base.First),
      (10,Base.First),
      (11,Base.Second),
      (17,Base.Third),
      (18,Base.Third),
      (4,Base.Home)
    ),
    Map(
      (2,Base.First),
      (9,Base.First),
      (10,Base.First),
      (11,Base.Second),
      (17,Base.Third),
      (18,Base.Third),
      (12,Base.Home)
    )
  )

  def getBaseFromRoll(roll:Int,hitter:Hitter): Base = {
    var base:Option[Base] = None
    if (hitter.Type == HitterType.Contact)
      base = ContactHitterSheet(hitter.Points-1).get(roll)
    else if (hitter.Type == HitterType.Power)
      base = PowerHitterSheet(hitter.Points-1).get(roll)

    if (base.isDefined)
      base.get
    else
      Base.None
  }
}
