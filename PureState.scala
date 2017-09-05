trait RNG {
    def nextInt: (Int, RNG);
}

object RNG {
    def simple(seed: Long): RNG = new RNG {
        def nextInt = {
            val seed2 = (seed*0x5DEECE66DL + 0xBL) &
                        ((1L << 48) - 1)
            ((seed2 >>> 16).asInstanceOf[Int],
            simple(seed2))      // Here our state is our value wrapped up in 
        }                       // a function 'simple'.
    }                           
    /**
    This allows us to take our returned state s = 
    / */
    def positiveInt(rng: RNG): (Int, RNG) = {
        val (a, s) = rng.nextInt 
        (a.abs, s)
    }
    def double(rng: RNG): (Double, RNG) = {
        val (a, s) = rng.nextInt 
        (a.abs, s)
    }
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (a1, s1) = rng.nextInt
        val (a2, s2) = double(s1)
        ((a1, a2), s2)
    }
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        if(count == 0)
            (List(), rng)
        else {
            val (a1, s1) = rng.nextInt
            val (as, s2) = ints(count - 1)(s1)
            (::(a1, as), s2)
        }
    }
}

object Pure {
    import RNG._
    val rng: RNG = simple(0)

}