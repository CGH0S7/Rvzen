// #include <stdio.h>

int main() {
  const unsigned int x = 1;
  const unsigned int y = 2;
  unsigned int z = x + y; // 写回z = 3
  if (z == 1) {           // 检测分支跳转指令
    z = z + 1;
  } else {
    z = z + 2;
    // z 最终结果为5
  }
  asm volatile("unimp");
  return 0;
}