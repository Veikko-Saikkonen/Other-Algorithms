/**
 * The class for elements.
 */
case class Element(val weight: Int, val value: Int) {
}

package object myKnapsack {
  

  /**
   * Given a maximum weight and a list of objects as (weight, value) pairs (Elements),
   * find the highest-value subset of the items that weighs at most maxWeight.
   * Uses dynamic programming and should be feasible for instances with many items but
   * relatively small maxWeight.
   */
  def solve(maxWeight: Int, items: Seq[Element]): (Int, Seq[Element]) = {
    // Check that all weights and values are positive
    items.foreach({case Element(weight, value) => {
      require(weight > 0)
      require(value > 0)
    }})
    
    val iArray = items.toArray
    val n = items.size
    val W = maxWeight
    val K = Array.ofDim[Int](n + 1, W + 1)
    val wt = iArray.map(_.weight)
    val vl = iArray.map(_.value)
    
    for(i <- 0 to n)
    {
      for(w <- 0 to W)
      {
        if(i*w == 0)
        {
          K(i)(w) = 0
        }
        else if(wt(i-1) <= w)
        {
          K(i)(w) = Math.max(vl(i-1) + K(i-1)(w-wt(i-1)), K(i-1)(w))
        }
        else K(i)(w) = K(i-1)(w)
      }
    }
    var w = W
    var i = n
    var res = K(n)(W)
    var resSeq = Array[Element]()
    
    while(i > 0 && res > 0)
    {
      if(res != K(i-1)(w))
      {
        resSeq :+= iArray(i - 1)
        res -= vl(i-1)
        w -= wt(i-1)
      }
      i -= 1
    }
    
  (K(n)(W), resSeq)
  }
}
