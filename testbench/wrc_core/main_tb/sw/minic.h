#ifndef __MINIC_H
#define __MINIC_H

#include "types.h"

#define ETH_HEADER_SIZE 14

void minic_init();
void minic_disable();
int minic_poll_rx();




#endif
