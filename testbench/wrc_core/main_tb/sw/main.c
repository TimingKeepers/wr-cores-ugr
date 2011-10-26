#include <stdio.h>
//#include <stdint.h>

#include "gpio.h"
#include "minic.h"

void _irq_entry(){}

int main(void)
{
	unsigned char frame [] = {1,2,3,4,5,6,7,8,9,10,11,12,13,14};

	uart_init();
	minic_init();
	minic_tx_frame(frame, 14);
	
	for(;;)
		uart_write_byte('U');
}



