package com.jk.pis

import com.jk.numbers.IntegerNumber
import com.jk.numbers.Zero
import com.jk.numbers.NaturalNumber

object Main {
def toN(n: Int): NaturalNumber = {
  def loop(acc: NaturalNumber, n: Int): NaturalNumber =  {
    if (n == 0) acc
    else if (n > 0) loop(acc.successor, n - 1)
    else throw new Exception
  }
  
  loop(Zero, n)
}

  def main(args: Array[String]) {
    val a = new IntegerNumber(toN(26), toN(13))
    val b = new IntegerNumber(toN(3), toN(0))

    println(a)
    println(b)
    println(a / b)
    println(a.toInt % b.toInt)
  }
}