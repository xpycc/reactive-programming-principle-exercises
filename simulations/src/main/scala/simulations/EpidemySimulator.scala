package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    // to complete: additional parameters of simulation
    val prevalenceRate = 0.01
    val transmissibilityRate = 0.4
    val deathRate = 0.25
    val maxDay = 5
    val beingSick = 6         // 6
    val decidingDeath = 8     // 14
    val beingImmune = 2       // 16
    val beingHealthy = 2      // 18

    val dx = Array( 0, 1, 0,-1)
    val dy = Array( 1, 0,-1, 0)
  }

  import SimConfig._

  val persons: List[Person] = { // to complete: construct list of persons
    val infPop = (population * prevalenceRate).toInt
    val f = 0 until infPop map { x => val p = new Person(x); p.infected = true; p}
    val s = infPop until population map (x => new Person(x))
    f.toList ::: s.toList
  }
  
  val gird = new Array[Array[Gird]](roomRows)
  
  class Gird(val row: Int, val col: Int) {
	var infectedCount = 0
	var visiblyInfectedCount = 0
      
    def addInfected(d: Int) {
      infectedCount += d
    }
      
    def addVisiblyInfected(d: Int) {
      visiblyInfectedCount += d
    }
    
    def visiblyInfected = visiblyInfectedCount > 0
    
    def infected = infectedCount > 0
    
    def neighbors = {
      for (i <- 0 until 4)
        yield gird((row + dx(i) + roomRows) % roomRows
          )((col + dy(i) + roomColumns) % roomColumns)
    }.toList
  }
  
  {			// class construction
    for (i <- 0 until roomRows)
      gird(i) = new Array[Gird](roomColumns)
    for {
      i <- 0 until roomRows
      j <- 0 until roomColumns
    } gird(i)(j) = new Gird(i, j)
    afterDelay(0) { register }
  }
  
  def register {
    for (p <- persons) p.firstCheck
    for (p <- persons if !p.dead)
      afterDelay(1 + randomBelow(maxDay)) { p.moveAction }
    afterDelay(0) { calcTransmission }
  }
  
  def calcTransmission {
    afterDelay(0)(realCalcTransmission)
  }
  
  def realCalcTransmission {
    for (p <- persons if !p.infected && !p.immune && gird(p.row)(p.col).infected) {
      if (random < transmissibilityRate) {
        p.getInfected
      }
    }
    afterDelay(1)(calcTransmission)
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    
    def firstCheck {
      if (dead) {
        gird(row)(col) addInfected 1
        gird(row)(col) addVisiblyInfected 1
        return
      }
      else if (immune) {
        gird(row)(col).addInfected(1)
        gird(row)(col).addVisiblyInfected(1)
        getImmune
      } else if (sick) {
        gird(row)(col).addInfected(1)
        getSick
      } else if (infected)
        getInfected
    }

    def moveAction {
      if (!sick)
        afterDelay(0)(realMoveAction)
      else
        realMoveAction
    }

    def realMoveAction {
      if (dead) return

      afterDelay(1 + randomBelow(maxDay))(moveAction)

      val vec = gird(row)(col).neighbors filter (!_.infected)

//      if (currentTime > 100) {
//        print(s"Gird ($row, $col)'s neighbor:")
//        for (i <- gird(row)(col).neighbors)
//          print(" " + i.visiblyInfectedCount)
//        println()
//      }
      
      if (vec.isEmpty) return
      
      val ch = vec(randomBelow(vec.size))
      if (infected) {
        gird(row)(col).addInfected(-1)
        ch.addInfected(1)
      }
      if (sick) {
        gird(row)(col).addVisiblyInfected(-1)
        ch.addVisiblyInfected(1)
      }
//      if (currentTime > 100)
//        println(s"person $id: ($row,$col) -> (${ch.row},${ch.col})")
      row = ch.row
      col = ch.col
    }
    
    def getInfected {
      infected = true
      gird(row)(col) addInfected 1
      afterDelay(beingSick)(getSick)
    }
    
    def getSick {
      sick = true
      gird(row)(col) addVisiblyInfected 1
      afterDelay(decidingDeath)(decideDeath)
    }
    
    def decideDeath {
      if (random < deathRate)
        dead = true
      else
        afterDelay(beingImmune)(getImmune)
    }
    
    def getImmune {
      immune = true
      sick = false
      gird(row)(col) addVisiblyInfected -1
      afterDelay(beingHealthy)(turnHealthy)
    }

    def turnHealthy {
      assert(!dead, "the")
      infected = false
      sick = false
      immune = false
      gird(row)(col).addInfected(-1)
    }
  }
}
