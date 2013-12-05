package scalajs

/**
 * emulate a Java-Long using three integers.
 * taken from gwt LongLib:
 * com.google.gwt.lang.LongLib
 *
 * holds values l, m, h (low, middle, high)
 * s.t. (x.l + ((long) x.m << 22) + ((long) x.h << 44)) is equal to
 * the original value
 */
final class Long private (
  val l: Int,
  val m: Int,
  val h: Int
) { x =>

  import Long._

  def unary_~ : Long = masked(~x.l, ~x.m, ~x.h)
  def unary_+ : Long = x
  def unary_- : Long = {
    val neg0 = (~x.l + 1) & MASK
    val neg1 = (~x.m + (if (neg0 == 0) 1 else 0)) & MASK
    val neg2 = (~x.h + (if (neg0 == 0 && neg1 == 0) 1 else 0)) & MASK_2
    Long(neg0, neg1, neg2)
  }

  def +(y: String): String = x.toString + y

  def <<(n_in: Int): Long = {
    /* crop MSB. Note: This will cause (2L << 65 == 2L << 1)
     * apparently this is as specified
     */
    val n = n_in & 63

    if (n < BITS) {
      val remBits = BITS - n
      masked(x.l << n,
             (x.m << n) | (x.l >> remBits),
             (x.h << n) | (x.m >> remBits))
    } else if (n < BITS01) {
      val shfBits = n - BITS
      val remBits = BITS01 - n
      masked(0, x.l << shfBits, (x.m << shfBits) | (x.l >> remBits))
    } else {
      masked(0, 0, x.l << (n - BITS01))
    }
    
  }

  /**
   * logical right shift
   */
  def >>>(n_in: Int): Long = {
    // Check that h is correctly masked, otherwise we'll shift values in
    assert(x.h == (x.h & MASK_2))
    val n = n_in & 63
    if (n < BITS) {
      val remBits = BITS - n
      masked((x.l >> n) | (x.m << remBits),
             // FIXME is this really >> and not >>>??
             (x.m >> n) | (x.h << remBits),
             x.h >>> n)
    } else if (n < BITS01) {
      val shfBits = n - BITS
      val remBits = BITS01 - n
             // FIXME is this really >> and not >>>??
      masked((x.m >> shfBits) | (x.h << remBits),
             x.h >>> shfBits, 0)
    } else
      masked(x.h >>> (n - BITS01), 0, 0)

  }

  /**
   * arithmetic right shift
   */
  def >>(n_in: Int): Long = {
    val n = n_in & 63;

    // Sign extend x.h
    val negative = (x.h & SIGN_BIT_VALUE) != 0
    val xh = if (negative) x.h | ~MASK_2 else x.h

    if (n < BITS) {
      val remBits = BITS - n
      // FIXME IMHO the first two >> should be >>>
      masked((x.l >> n) | (x.m << remBits),
             (x.m >> n) | (xh  << remBits),
             xh >> n)
    } else if (n < BITS01) {
      val shfBits = n - BITS
      val remBits = BITS01 - n
      // FIXME IMHO the first >> should be >>>
      masked((x.m >> shfBits) | (xh << remBits),
              xh  >> shfBits,
             if (negative) MASK_2 else 0)
    } else {
      masked(xh >> (n - BITS01),
             if (negative) MASK   else 0,
             if (negative) MASK_2 else 0)
    }

  }

  def ==(y: Long): Boolean = x.l == y.l && x.m == y.m && x.h == y.h
  def !=(y: Long): Boolean = x.l != y.l || x.m != y.m || x.h != y.h

  def +(y: Long) = {
    val sum0 = x.l + y.l
    val sum1 = x.m + y.m + (sum0 >> BITS)
    val sum2 = x.h + y.h + (sum1 >> BITS)
    masked(sum0, sum1, sum2)
  }


  def &(y: Long): Long = Long(x.l & y.l, x.m & y.m, x.h & y.h)

  


}


object Long {

  private val BITS:    Int = 22
  private val BITS01:  Int = 2 * BITS
  private val BITS2:   Int = 64 - BITS01
  private val MASK:    Int = (1 << BITS) - 1
  private val MASK_2:  Int = (1 << BITS2) - 1

  private val SIGN_BIT:       Int    = BITS2 - 1
  private val SIGN_BIT_VALUE: Int    = 1 << SIGN_BIT
  private val TWO_PWR_15_DBL: Double = 0x8000
  private val TWO_PWR_16_DBL: Double = 0x10000
  private val TWO_PWR_22_DBL: Double = 0x400000
  private val TWO_PWR_31_DBL: Double = TWO_PWR_16_DBL * TWO_PWR_15_DBL
  private val TWO_PWR_32_DBL: Double = TWO_PWR_16_DBL * TWO_PWR_16_DBL
  private val TWO_PWR_44_DBL: Double = TWO_PWR_22_DBL * TWO_PWR_22_DBL
  private val TWO_PWR_63_DBL: Double = TWO_PWR_32_DBL * TWO_PWR_31_DBL

  protected def apply(value: Int) = {
    val a0 = value & MASK
    val a1 = (value >> BITS) & MASK
    val a2 = if (value < 0) MASK_2 else 0
    new Long(a0, a1, a2)
  }

  protected def masked(l: Int, m: Int, h: Int) =
    Long(l & MASK, m & MASK, h & MASK_2)
  protected def apply(l: Int, m: Int, h: Int) = new Long(l, m, h)
  
}
