#ifndef __BOARD_H
#define __BOARD_H

#define BASE_GPIO 	0x60100

static inline int delay(int x)
{
  while(x--) asm volatile("nop");
}
  
#endif
