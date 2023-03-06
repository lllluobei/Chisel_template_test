package LU_Decomposition

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import scala.math.pow

class fix_mm(In_Vec_len:Int, width:Int, point:Int) extends Module {
  val fptype = FixedPoint(width.W,point.BP)
  val io = IO(new Bundle{
    val in_mul = Input(Vec(In_Vec_len, fptype))
    val in_seq = Input(Vec(In_Vec_len, fptype))
    val out = Output(fptype)
  })

  def InverseTable:Vec[FixedPoint] = {
    val inverse = Range(0, 1 << width)
      .map( i => i/pow(2.0,point) )
      .map( i => if(i != 0) 1.0/i else 0 )
      .map( a => FixedPoint.fromDouble(a,width.W,point.BP) )
    VecInit(inverse)
  }

  io.out := io.in_mul.zip(io.in_seq).map{ case(a,b) => a*b }.reduce( _+_ )

  printf("input_seq ")
  for (j <- 0 until In_Vec_len) { printf("%d ",io.in_seq(j).asUInt) }
  printf("\n")
  printf("input_mul ")
  for (j <- 0 until In_Vec_len) { printf("%d ",io.in_mul(j).asUInt) }
  printf("\n")
  printf("Out%d ",io.out.asUInt)
  printf("\n")
}


