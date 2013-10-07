package com.jk.numbers

class RationalNumber(a: IntegerNumber, b: IntegerNumber) {
  val numer = a
  val denom = b
  
  override def toString = numer + "/" + denom 
}