package com.jk.pis

class SkuVelocity(s: String, v: Double) {
	val skuId = s
	val velocity = v
	
	override def toString = s + ", " + v
}