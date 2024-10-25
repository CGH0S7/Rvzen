    .section .text
    .globl _start

_start:
    addi x1, x0, 10     # 将 10 加载到寄存器 x1 中
    addi x2, x0, 20     # 将 20 加载到寄存器 x2 中
    add  x3, x1, x2     # 执行 add 指令，将 x1 和 x2 的值相加，结果保存在 x3 中
    nop                 # 保持空操作，防止之后有未定义行为
