package common

import chisel3._
import chisel3.util._

object Consts {
  // 数据宽度和地址
  val WORD_LEN = 32 // 指令和数据的宽度为32位
  val START_ADDR = 0.U(WORD_LEN.W) // 起始地址，设为0
  val BUBBLE = 0x00000013.U(WORD_LEN.W) // 用于冒泡的指令 [ADDI x0, x0, 0]
  // val UNIMP = 0xc0001073L.U(WORD_LEN.W) // 未实现指令 [CSRRW x0, cycle, x0]
  val UNIMP = "x_c0001073".U(WORD_LEN.W)

  // 寄存器地址长度
  val ADDR_LEN = 5 // rs1、rs2和写回寄存器的地址宽度为5位
  val CSR_ADDR_LEN = 12 // CSR寄存器地址宽度为12位

  // 矢量寄存器参数
  val VLEN = 128 // 矢量寄存器长度为128位
  val LMUL_LEN = 2 // LMUL字段长度
  val SEW_LEN = 11 // SEW字段长度
  val VL_ADDR = 0xc20 // VL寄存器地址
  val VTYPE_ADDR = 0xc21 // VTYPE寄存器地址

  // 执行功能定义
  val EXE_FUN_LEN = 5 // 执行功能的编码长度为5位
  val ALU_X = 0.U(EXE_FUN_LEN.W) // 未定义ALU操作
  val ALU_ADD = 1.U(EXE_FUN_LEN.W) // 加法操作
  val ALU_SUB = 2.U(EXE_FUN_LEN.W) // 减法操作
  val ALU_AND = 3.U(EXE_FUN_LEN.W) // 位与操作
  val ALU_OR = 4.U(EXE_FUN_LEN.W) // 位或操作
  val ALU_XOR = 5.U(EXE_FUN_LEN.W) // 位异或操作
  val ALU_SLL = 6.U(EXE_FUN_LEN.W) // 左移操作
  val ALU_SRL = 7.U(EXE_FUN_LEN.W) // 逻辑右移操作
  val ALU_SRA = 8.U(EXE_FUN_LEN.W) // 算术右移操作
  val ALU_SLT = 9.U(EXE_FUN_LEN.W) // 有符号比较小于操作
  val ALU_SLTU = 10.U(EXE_FUN_LEN.W) // 无符号比较小于操作
  val BR_BEQ = 11.U(EXE_FUN_LEN.W) // 分支相等
  val BR_BNE = 12.U(EXE_FUN_LEN.W) // 分支不等
  val BR_BLT = 13.U(EXE_FUN_LEN.W) // 有符号小于分支
  val BR_BGE = 14.U(EXE_FUN_LEN.W) // 有符号大于等于分支
  val BR_BLTU = 15.U(EXE_FUN_LEN.W) // 无符号小于分支
  val BR_BGEU = 16.U(EXE_FUN_LEN.W) // 无符号大于等于分支
  val ALU_JALR = 17.U(EXE_FUN_LEN.W) // JALR跳转
  val ALU_COPY1 = 18.U(EXE_FUN_LEN.W) // 复制操作
  val ALU_VADDVV = 19.U(EXE_FUN_LEN.W) // 矢量加法操作
  val VSET = 20.U(EXE_FUN_LEN.W) // 设置矢量寄存器
  val ALU_PCNT = 21.U(EXE_FUN_LEN.W) // 位计数操作
  val ALU_PCZD = 22.U(EXE_FUN_LEN.W) // 位清零操作
  val ALU_MUL = 23.U(EXE_FUN_LEN.W) // 乘法操作
  val ALU_DIV = 24.U(EXE_FUN_LEN.W) // 除法操作
  val ALU_MULH = 25.U(EXE_FUN_LEN.W) // 乘法高位操作
  val ALU_MULHSU = 26.U(EXE_FUN_LEN.W) // 乘法高位有符号操作
  val ALU_MULHU = 27.U(EXE_FUN_LEN.W) // 乘法高位无符号操作
  val ALU_DIVU = 28.U(EXE_FUN_LEN.W) // 除法无符号操作
  val ALU_REM = 29.U(EXE_FUN_LEN.W) // 取余操作
  val ALU_REMU = 30.U(EXE_FUN_LEN.W) // 取余无符号操作

  // 操作数选择
  val OP1_LEN = 2 // 操作数1的选择宽度为2位
  val OP1_RS1 = 0.U(OP1_LEN.W) // 选择rs1
  val OP1_PC = 1.U(OP1_LEN.W) // 选择PC
  val OP1_X = 2.U(OP1_LEN.W) // 未定义操作数1
  val OP1_IMZ = 3.U(OP1_LEN.W) // 立即数操作数1

  val OP2_LEN = 3 // 操作数2的选择宽度为3位
  val OP2_X = 0.U(OP2_LEN.W) // 未定义操作数2
  val OP2_RS2 = 1.U(OP2_LEN.W) // 选择rs2
  val OP2_IMI = 2.U(OP2_LEN.W) // 立即数操作数2（immI）
  val OP2_IMS = 3.U(OP2_LEN.W) // 立即数操作数2（immS）
  val OP2_IMJ = 4.U(OP2_LEN.W) // 立即数操作数2（immJ）
  val OP2_IMU = 5.U(OP2_LEN.W) // 立即数操作数2（immU）

  // 内存访问使能信号
  val MEN_LEN = 2 // 内存访问控制宽度为2位
  val MEN_X = 0.U(MEN_LEN.W) // 未定义
  val MEN_S = 1.U(MEN_LEN.W) // 存储操作
  val MEN_V = 2.U(MEN_LEN.W) // 矢量存储操作

  // 寄存器使能信号
  val REN_LEN = 2 // 寄存器使能信号宽度为2位
  val REN_X = 0.U(REN_LEN.W) // 未定义
  val REN_S = 1.U(REN_LEN.W) // 标量寄存器写回使能
  val REN_V = 2.U(REN_LEN.W) // 矢量寄存器写回使能

  // 写回选择
  val WB_SEL_LEN = 3 // 写回选择信号宽度为3位
  val WB_X = 0.U(WB_SEL_LEN.W) // 未定义
  val WB_ALU = 0.U(WB_SEL_LEN.W) // ALU结果写回
  val WB_MEM = 1.U(WB_SEL_LEN.W) // 内存数据写回
  val WB_PC = 2.U(WB_SEL_LEN.W) // PC写回
  val WB_CSR = 3.U(WB_SEL_LEN.W) // CSR寄存器写回
  val WB_MEM_V = 4.U(WB_SEL_LEN.W) // 矢量内存数据写回
  val WB_ALU_V = 5.U(WB_SEL_LEN.W) // 矢量ALU结果写回
  val WB_VL = 6.U(WB_SEL_LEN.W) // VL寄存器写回

  // 内存宽度
  val MW_LEN = 3 // 内存宽度控制信号宽度为3位
  val MW_X = 0.U(MW_LEN.W) // 未定义
  val MW_W = 1.U(MW_LEN.W) // 32位字访问
  val MW_H = 2.U(MW_LEN.W) // 16位半字访问
  val MW_B = 3.U(MW_LEN.W) // 8位字节访问
  val MW_HU = 4.U(MW_LEN.W) // 16位无符号半字访问
  val MW_BU = 5.U(MW_LEN.W) // 8位无符号字节访问

  // CSR操作类型
  val CSR_LEN = 3 // CSR操作类型宽度为3位
  val CSR_X = 0.U(CSR_LEN.W) // 未定义
  val CSR_W = 1.U(CSR_LEN.W) // 写入操作
  val CSR_S = 2.U(CSR_LEN.W) // 设置操作
  val CSR_C = 3.U(CSR_LEN.W) // 清除操作
  val CSR_E = 4.U(CSR_LEN.W) // 特权指令执行
  val CSE_V = 5.U(CSR_LEN.W) // 矢量CSR操作

}
