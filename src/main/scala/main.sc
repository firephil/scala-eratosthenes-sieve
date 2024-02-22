def sieveOfEratosthenes(n: Int): Seq[Int] = {
  val isPrime = Array.fill(n + 1)(true) // Initially assume all are primes
  isPrime(0) = false
  isPrime(1) = false

  for (p <- 2 to Math.sqrt(n).toInt) {
    if (isPrime(p)) {
      // Mark multiples of p as composite starting from p^2
      for (i <- p * p to n by p) isPrime(i) = false
    }
  }

  // Filter the remaining prime numbers
  isPrime.zipWithIndex.filter(_._1).map(_._2)
}