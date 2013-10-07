package com.jk.numbers

trait NaturalNumber {
  def successor = Successor(this)
  def + (that: NaturalNumber): NaturalNumber
  def - (that: NaturalNumber): NaturalNumber
  def * (that: NaturalNumber): NaturalNumber
  def / (that: NaturalNumber): NaturalNumber
  def % (that: NaturalNumber): NaturalNumber
  
  def < (that: NaturalNumber): Boolean
  def > (that: NaturalNumber) = that < this
  def <= (that: NaturalNumber) = (this == that) || (this < that)
  def >= (that: NaturalNumber) = (this == that) || (this > that)
  
  def max(that: NaturalNumber) = if (this < that) that else this
  def min(that: NaturalNumber) = if (this < that) this else that
  
  def toInt: Int
  override def toString = this.toInt.toString
}

case object Zero extends NaturalNumber {
  def + (that: NaturalNumber) = that
  def - (that: NaturalNumber) = that match {
    case Zero => Zero
    case _ => throw new Exception
  }
  
  def * (that: NaturalNumber) = Zero
  def / (that: NaturalNumber) = that match {
    case Zero => throw new Exception("Division by zero")
    case _ => Zero
  }
  def % (that: NaturalNumber) = that match {
    case Zero => throw new Exception("Modulus by zero")
    case _ => Zero
  }
  
  def < (that: NaturalNumber) = that match {
    case Zero => false
    case _ => true
  }
 
  def toInt = 0
}

case class Successor(predecessor: NaturalNumber) extends NaturalNumber {
  require(predecessor != null, "Predecessor can't be null")
  
  def + (that: NaturalNumber) = (predecessor + that).successor
  
  def - (that: NaturalNumber) = that match {
    case Zero => Successor.this
    case Successor(thatPredecessor) => predecessor - thatPredecessor
  }
  
  def * (that: NaturalNumber) = (predecessor * that) + that
  
  def / (that: NaturalNumber) = that match {
    case Zero => throw new Exception("Division by zero")
    case _ => {
      def loop(acc: NaturalNumber, rest: NaturalNumber): NaturalNumber = {
        if (rest < that) acc
        else loop(acc.successor, rest - that)
      }
      
      loop(Zero, Successor.this)
    }
  }
  
  def % (that: NaturalNumber) = that match {
    case Zero => throw new Exception("Modulus by zero")
    case _ => {
      def loop(rest: NaturalNumber): NaturalNumber = {
        if (rest < that) rest
        else loop(rest - that)
      }
      
      loop(Successor.this)
    }
  }
  
  def < (that: NaturalNumber) = that match {
    case Zero => false
    case Successor(thatPredecessor) => predecessor < thatPredecessor
  }
  
  override def toInt = {
    def loop(acc: Int, rest: NaturalNumber): Int = rest match {
      case Zero => acc
      case Successor(thatPredecessor) => loop(acc + 1, thatPredecessor)
    }
    
    loop(0, Successor.this)
  }
}