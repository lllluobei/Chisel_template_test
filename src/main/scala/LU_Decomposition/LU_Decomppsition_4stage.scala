package LU_Decomposition

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

import scala.math.pow

class Matrix_inversion_4stage(In_Vec_len:Int, width:Int, point:Int) extends Module {
  val fptype = FixedPoint(width.W,point.BP)
  val io = IO(new Bundle{
    val in_seq = Input(Vec(In_Vec_len, fptype))
    val LU_state = Input(UInt(3.W))
    val re_stage = Output(UInt(3.W))
    val out_seq = Output(Vec(In_Vec_len, fptype))
    })

  def InverseTable_pos:Vec[FixedPoint] = {
    val inverse_p = Range(1 << 2, 1 << (width - point/2), 1)
      .map( i => i/pow(2.0,point) )
      .map( i => if(i != 0.0) 1.0/i else 0 )
      .map( a => FixedPoint.fromDouble(a,width.W,point.BP) )
    VecInit(inverse_p)
  }

  /*
  def InverseTable_nag:Vec[FixedPoint] = {
    val inverse_n = Range(-(1 << (width - point / 2 -1)), -(1 << 2), 1)
      .map(i => i / pow(2.0, point))
      .map(i => if (i != 0.0) 1.0 / i else 0)
      .map(a => FixedPoint.fromDouble(a, width.W, point.BP))
    VecInit(inverse_n)
  }
  */

  val idle :: clear :: load :: matrix_update :: construct :: backward :: out :: Nil = Enum(7)
  val stateReg = RegInit(clear)
  val original_matrix = RegInit(VecInit(Seq.fill(In_Vec_len)( VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromBigInt(0,width.W, point.BP)))) ))
  val internal_matrix = VecInit(Seq.fill(In_Vec_len)( VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromDouble(0.0,width.W, point.BP)))) )
  val update_matrix = VecInit(Seq.fill(In_Vec_len)( VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromDouble(0.0,width.W, point.BP)))) )
  val L_matrix = RegInit(VecInit(Seq.fill(In_Vec_len)( VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromDouble(0.0,width.W, point.BP)))) ))
  val U_matrix = RegInit(VecInit(Seq.fill(In_Vec_len)( VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromDouble(0.0,width.W, point.BP)))) ))
  val P_values = RegInit(VecInit( Seq.fill(In_Vec_len)(0.U((log2Ceil(In_Vec_len)+1).W)) ))
  val internal_P = VecInit( Seq.fill(In_Vec_len)(0.U((log2Ceil(In_Vec_len)+1).W)) )
  val out_matirx = RegInit(VecInit(Seq.fill(In_Vec_len)( VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromDouble(0.0,width.W, point.BP)))) ))
  val out_conlumn = VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromDouble(0.0,width.W, point.BP)))
  val pat_out = VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromBigInt(0,width.W, point.BP)))
  val row_count = Counter(In_Vec_len + 1)

  //printf("now_stage %d \n",stateReg)
  stateReg := io.LU_state
  io.re_stage := stateReg
  switch(stateReg) {
    is(idle) {
      when(io.LU_state =/= 1.U){
      for (i <- 0 until In_Vec_len) {
        for (j <- 0 until In_Vec_len) {
          original_matrix(i)(j) := original_matrix(i)(j)
          L_matrix(i)(j) := L_matrix(i)(j)
          U_matrix(i)(j) := U_matrix(i)(j)
          out_matirx(i)(j) := out_matirx(i)(j)
        }
        row_count.value := row_count.value
      }
      stateReg := stateReg
      }.otherwise{
        stateReg := clear
        row_count.value := 0.U
      }
    }
    is(clear) {
      for (i <- 0 until In_Vec_len) {
        P_values(i) := 0.U
        for (j <- 0 until In_Vec_len) {
          original_matrix(i)(j) := FixedPoint.fromBigInt(0, width.W, point.BP)
          L_matrix(i)(j) := FixedPoint.fromBigInt(0, width.W, point.BP)
          U_matrix(i)(j) := FixedPoint.fromBigInt(0, width.W, point.BP)
          out_matirx(i)(j) := FixedPoint.fromBigInt(0, width.W, point.BP)
        }
      }
      row_count.value := 0.U
      stateReg := load
    }
    is(load) {
      when(row_count.value < (In_Vec_len-1).U) {
        for (i <- 0 until In_Vec_len) original_matrix(row_count.value)(i) := io.in_seq(i)
        row_count.inc()
        stateReg := stateReg
      }
      .otherwise {
        for (i <- 0 until In_Vec_len) original_matrix(row_count.value)(i) := io.in_seq(i)
        stateReg := matrix_update
        row_count.value := 0.U
      }
    }
    is(matrix_update) {
      when(row_count.value < (In_Vec_len - 1).U) {
        val pick_column = VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromBigInt(0, width.W, point.BP)))
        Range(0, In_Vec_len, 1)
          .foreach { a =>
            when(a.U >= row_count.value) {
              pick_column(a) := original_matrix(a)(row_count.value)
            }
              .otherwise {
                pick_column(a) := FixedPoint.fromDouble(0.0,width.W,point.BP)
              }
          }


        /*
          printf("Pick_column ")
          for (i <- 0 until In_Vec_len) {
            printf("%d ", pick_column(i).asUInt)
          }
          printf("\n")
         */

        val local_max_ind = pick_column.indexWhere(_ === pick_column.reduce((a, b) => Mux(a.abs > b.abs, a, b)))
        Range(0, In_Vec_len, 1).foreach{ idx =>
          when(row_count.value === 0.U) {
            when(idx.U === row_count.value) {
              internal_P(idx) := local_max_ind
            }
              .elsewhen(idx.U === local_max_ind) {
                internal_P(idx) := row_count.value
              }
              .otherwise {
                internal_P(idx) := idx.U
              }
          }
            .otherwise {
              when(local_max_ind > row_count.value) {
                when(idx.U === row_count.value) {
                  internal_P(idx) := P_values(local_max_ind)
                }
                  .elsewhen(idx.U === local_max_ind) {
                    internal_P(idx) := P_values(row_count.value)
                  }
                  .otherwise {
                    internal_P(idx) := P_values(idx)
                  }
              }
                .otherwise {
                  internal_P := P_values
                }
            }
        }
        /*
        printf("Local_max ")
        printf("%d ", local_max_ind)
        printf("\n")
         */

        Range(0, In_Vec_len, 1).foreach { idx =>
          when(idx.U === row_count.value) {
            internal_matrix(idx) := original_matrix(local_max_ind)
          }
            .elsewhen(idx.U === local_max_ind) {
              internal_matrix(idx) := original_matrix(row_count.value)
            }
            .otherwise {
              internal_matrix(idx) := original_matrix(idx)
            }
        }

        Range(0, In_Vec_len, 1).foreach { j =>
          when(j.U > row_count.value) {
            val scale = internal_matrix(j)(row_count.value) * Mux(internal_matrix(row_count.value)(row_count.value)>FixedPoint.fromDouble(0.0,width.W,point.BP),InverseTable_pos(internal_matrix(row_count.value)(row_count.value).asUInt),- InverseTable_pos(internal_matrix(row_count.value)(row_count.value).abs.asUInt))
            update_matrix(j)(row_count.value) := scale
            Range(0, In_Vec_len, 1).foreach { k =>
              when(k.U > row_count.value) {
                val new_value = internal_matrix(j)(k) - internal_matrix(row_count.value)(k) * scale
                update_matrix(j)(k) := new_value
              }.elsewhen(k.U < row_count.value) {
                update_matrix(j)(k) := internal_matrix(j)(k)
              }
            }
          }.otherwise {
            update_matrix(j) := internal_matrix(j)
          }
        }

        /*
        printf("update_matrixs\n")
        for (i <- 0 until In_Vec_len) {
          for (j <- 0 until In_Vec_len) {
            printf("%d ", update_matrix(i)(j).asUInt)
          }
          printf("\n")
        }
        printf("\n")
         */

        for (j <- 0 until In_Vec_len) {
          P_values(j) := internal_P(j)
          original_matrix(j) := update_matrix(j)
        }

        row_count.inc()
        stateReg := stateReg
      }
        .otherwise {
          for (i <- 0 until In_Vec_len) {
            original_matrix(i) := original_matrix(i)
          }
          stateReg := construct
          row_count.value := 0.U
        }
    }
    is(construct) {
      for(a <- 0 until In_Vec_len) {
        for(b <- 0 until In_Vec_len) {
          if (a > b) {
            L_matrix(a)(b) := original_matrix(a)(b)
            U_matrix(a)(b) := U_matrix(a)(b)
          }
          else if (a == b) {
            L_matrix(a)(b) := FixedPoint.fromDouble(1.0, width.W, point.BP)
            U_matrix(a)(b) := original_matrix(a)(b)
          }
          else {
            U_matrix(a)(b) := original_matrix(a)(b)
            L_matrix(a)(b) := L_matrix(a)(b)
          }
        }
      }

      stateReg := backward
      row_count.value := 0.U

    }
    is(backward){
      when(row_count.value < (In_Vec_len).U){

        val b_col = VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromBigInt(0,width.W, point.BP)))
        b_col(row_count.value) := FixedPoint.fromDouble(1.0,width.W,point.BP)
        val internal_y = VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromBigInt(0,width.W, point.BP)))
        for( i <- 0 until In_Vec_len ){ internal_y(i) := b_col(P_values(i)) }

        val out_internal_y = VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromBigInt(0,width.W, point.BP)))
        Range(0,In_Vec_len,1).foreach{idx =>
          when(idx.U === 0.U){ out_internal_y(0) := internal_y(0) }
            .elsewhen(idx.U === 1.U){ out_internal_y(1) := internal_y(1) - L_matrix(1)(0) * out_internal_y(0) }
            .elsewhen(idx.U > 1.U){
              val p_sum = L_matrix(idx).zip(out_internal_y).zipWithIndex.map{ case((a, b),ix) =>
                if(ix < idx) a*b
                else FixedPoint.fromDouble(0.0,width.W,point.BP)
              }.reduce(_+_)
              out_internal_y(idx) := internal_y(idx) - p_sum
            }
        }

        val internal_x = VecInit(Seq.fill(In_Vec_len)(FixedPoint.fromBigInt(0,width.W, point.BP)))
        Range(0, In_Vec_len, 1).reverse.foreach{ idx =>
          when(idx.U === (In_Vec_len-1).U ){ internal_x(In_Vec_len-1) := out_internal_y(In_Vec_len-1) * Mux(U_matrix(In_Vec_len-1)(In_Vec_len-1)>FixedPoint.fromDouble(0.0,width.W,point.BP),InverseTable_pos(U_matrix(In_Vec_len-1)(In_Vec_len-1).asUInt), -InverseTable_pos(U_matrix(In_Vec_len-1)(In_Vec_len-1).abs.asUInt)) }
          .elsewhen(idx.U === (In_Vec_len-2).U){ internal_x(In_Vec_len-2) := (out_internal_y(In_Vec_len-2) - U_matrix(In_Vec_len-2)(In_Vec_len-1) * internal_x(In_Vec_len-1)) * Mux(U_matrix(In_Vec_len-2)(In_Vec_len-2)>FixedPoint.fromDouble(0.0,width.W,point.BP),InverseTable_pos(U_matrix(In_Vec_len-2)(In_Vec_len-2).asUInt), -InverseTable_pos(U_matrix(In_Vec_len-2)(In_Vec_len-2).abs.asUInt)) }
            .otherwise{
              val p_sum = U_matrix(idx).zip(internal_x).zipWithIndex.map{ case((a, b),ix) =>
                if(ix > idx) a*b
                else FixedPoint.fromDouble(0.0,width.W,point.BP)
              }.reduce(_+_)
              internal_x(idx) := (out_internal_y(idx) - p_sum) * Mux(U_matrix(idx)(idx) > FixedPoint.fromDouble(0.0,width.W,point.BP),InverseTable_pos(U_matrix(idx)(idx).asUInt), -InverseTable_pos(U_matrix(idx)(idx).abs.asUInt))
            }
        }
        for( i <- 0 until In_Vec_len ){ out_matirx(i)(row_count.value) := internal_x(i) }

        row_count.inc()
        stateReg := stateReg


        /*
        printf("L_matrix\n")
        for (i <- 0 until In_Vec_len) {
          for (j <- 0 until In_Vec_len) {
            printf("%d ", L_matrix(i)(j).asUInt)
          }
          printf("\n")
        }
        printf("\n")

        printf("U_matrix\n")
        for (i <- 0 until In_Vec_len) {
          for (j <- 0 until In_Vec_len) {
            printf("%d ", U_matrix(i)(j).asUInt)
          }
          printf("\n")
        }
        printf("\n")

        printf("P ")
        for( i <- 0 until In_Vec_len ){ printf("%d ", P_values(i).asUInt) }
        printf("\n")

        printf("b ")
        for( i <- 0 until In_Vec_len ){ printf("%d ", b_col(i).asUInt) }
        printf("\n")

        printf("internal_y ")
        for( i <- 0 until In_Vec_len ){ printf("%d ", internal_y(i).asUInt) }
        printf("\n")

        printf("out_internal_y ")
        for( i <- 0 until In_Vec_len ){ printf("%d ", out_internal_y(i).asUInt) }
        printf("\n")

        printf("out_internal_x ")
        for( i <- 0 until In_Vec_len ){ printf("%d ", internal_x(i).asUInt) }
        printf("\n")

        printf("internal_OUT_matrix\n")
        for (i <- 0 until In_Vec_len) {
          for (j <- 0 until In_Vec_len) {
            printf("%d ", out_matirx(i)(j).asUInt)
          }
          printf("\n")
        }
        printf("\n")

         */

      }
        .otherwise{
          stateReg := out
          row_count.value := 0.U

          /*
          printf("final_OUT_matrix\n")
          for (i <- 0 until In_Vec_len) {
            for (j <- 0 until In_Vec_len) {
              printf("%d ", out_matirx(i)(j).asUInt)
            }
            printf("\n")
          }
          printf("\n")
           */


        }
    }
    is(out) {
      when( row_count.value < In_Vec_len.U ) {
        for (i <- 0 until In_Vec_len) pat_out(i) := out_matirx(row_count.value)(i)
        row_count.inc()
        stateReg := stateReg
      }
      .otherwise {
        stateReg := idle
        row_count.value := 0.U
      }
    }
  }

    io.out_seq := pat_out
}

object Module_Gen_4stage extends App { (new chisel3.stage.ChiselStage).emitVerilog(new Matrix_inversion_4stage(4,16,8)) }
