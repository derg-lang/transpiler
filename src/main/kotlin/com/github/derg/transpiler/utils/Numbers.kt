package com.github.derg.transpiler.utils

import java.math.BigInteger

operator fun Int.plus(that: BigInteger): BigInteger = this.toBigInteger() + that
operator fun Long.plus(that: BigInteger): BigInteger = this.toBigInteger() + that
operator fun BigInteger.plus(that: Int): BigInteger = this + that.toBigInteger()
operator fun BigInteger.plus(that: Long): BigInteger = this + that.toBigInteger()

operator fun Int.minus(that: BigInteger): BigInteger = this.toBigInteger() - that
operator fun Long.minus(that: BigInteger): BigInteger = this.toBigInteger() - that
operator fun BigInteger.minus(that: Int): BigInteger = this - that.toBigInteger()
operator fun BigInteger.minus(that: Long): BigInteger = this - that.toBigInteger()
