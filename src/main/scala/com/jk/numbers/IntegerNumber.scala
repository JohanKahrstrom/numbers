package com.jk.numbers

class IntegerNumber(a: NaturalNumber, b: NaturalNumber) {
  private val m = a min b
  
  // Either pos or neg is Zero
  val pos = a - m
  val neg = b - m
  
  def this(a: NaturalNumber) = this(a, Zero)
  
  def abs = pos max neg
  
  def + (that: IntegerNumber) = new IntegerNumber(pos + that.pos, neg + that.neg)
  def - (that: IntegerNumber) = new IntegerNumber(pos + that.neg, neg + that.pos)
  def unary_- = new IntegerNumber(neg, pos)
  
  def * (that: IntegerNumber) = new IntegerNumber(pos * that.pos + neg * that.neg, neg * that.pos + pos * that.neg)
  
  def / (that: IntegerNumber): IntegerNumber = {
    if (that.neg == that.pos) throw new Exception("Division by zero")
    else {
      val denom = that.pos * that.pos + that.neg * that.neg - (Zero.successor.successor) * that.pos * that.neg
      new IntegerNumber((pos * that.pos + neg * that.neg) / denom, (pos * that.neg + neg * that.pos) / denom)
    }
  }
  
  def % (that: IntegerNumber) = (this - (this / that) * that)
  
  def == (that: IntegerNumber) = (pos + that.neg) == (neg + that.pos)
  def < (that: IntegerNumber) = (pos + that.neg) < (that.pos + neg) 
  def > (that: IntegerNumber) = that < this
  def <= (that: IntegerNumber) = (this == that) || (this < that)
  def >= (that: IntegerNumber) = (this == that) || (this > that)
  
  def max(that: IntegerNumber) = if (this < that) that else this
  def min(that: IntegerNumber) = if (this < that) this else that
  
  def toInt = if (pos < neg) -(neg - pos).toInt else (pos - neg).toInt
  override def toString = this.toInt.toString
}