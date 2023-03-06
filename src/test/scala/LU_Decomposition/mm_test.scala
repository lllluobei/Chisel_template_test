package LU_Decomposition

import chisel3._
import org.scalatest._
import chiseltest._
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import chisel3.experimental.FixedPoint
import scala.util.Random
import scala.math.pow

class mm_test extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  behavior of "fixpoint matrix multiplication"
  it should "produce right output" in {
    test(new fix_mm(2,16,8)){ c =>
      val r = new Random()

      for(j <- 0 until 3){
         val input_seq1 = Seq.fill(2)(r.nextFloat())
         val input_seq2 = Seq.fill(2)(-r.nextFloat())
         for(i <- 0 until 2){
           c.io.in_seq(i).poke(FixedPoint.fromDouble( input_seq1(i),16.W,8.BP ))
           c.io.in_mul(i).poke(FixedPoint.fromDouble( input_seq2(i),16.W,8.BP ))
         }
        c.clock.step(1)
       }

    }
  }
}
