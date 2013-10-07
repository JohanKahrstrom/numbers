package com.jk.pis

class Rational(n: Int, d: Int) {
  require(d != 0, "Denominator must be non-zero")
  
  private val g = gcd(n.abs, d)
  val numer: Int = n / g
  val denom: Int = d / g
  
  def this(n: Int) = this(n, 1)
  
  def unary_- = new Rational(-numer, denom)
  
  def + (that: Rational): Rational = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
  def + (i: Int): Rational = new Rational(numer + i * denom, denom)
  
  def - (that: Rational): Rational = new Rational(numer * that.denom - that.numer * denom, denom * that.denom)
  def - (i: Int): Rational = new Rational(numer - i * denom, denom)
  
  def * (that: Rational): Rational = new Rational(numer * that.numer, denom * that.denom)
  def * (i: Int): Rational = new Rational(numer * i, denom)
  
  def / (that: Rational): Rational = new Rational(numer * that.denom, denom * that.numer)
  def / (i: Int): Rational = new Rational(numer, denom * i)
  
  def inv = new Rational(this.denom, this.numer)
  
  def == (that: Rational) = this.numer * that.denom == this.denom * that.numer
  def < (that: Rational) = this.numer * that.denom < that.numer * this.denom  
  def <= (that: Rational) = if (this == that) true else this < that  
  def > (that: Rational) = this.numer * that.denom > that.numer * this.denom
  def >= (that: Rational) = if (this == that) true else this > that
  
  def max(that: Rational) = if (this < that) that else this
  
  override def toString = if (this.denom == 1) this.numer.toString else this.numer + "/" + this.denom
  
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  private def sign(a: Int) = if (a < 0) -1 else 1
}