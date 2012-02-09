#include <stdio.h>

#include "types.h"
#include "minic.h"

#include <string.h>
#include <hw/minic_regs.h>
#include "endpoint_regs.h"

#define BASE_MINIC 0x10000
#define BASE_EP 0x10100

#define MINIC_DMA_TX_BUF_SIZE 1024

#define TX_DESC_VALID  (1<<31)
#define TX_DESC_WITH_OOB (1<<30)
#define TX_DESC_HAS_OWN_MAC (1<<28)

#define MINIC_MTU 256

static volatile uint32_t dma_tx_buf[MINIC_DMA_TX_BUF_SIZE / 4];

struct wr_minic {
  volatile uint32_t *rx_head, *rx_base; 
  uint32_t rx_avail, rx_size;
  volatile uint32_t *tx_head, *tx_base;
  uint32_t tx_avail, tx_size;

  int synced;
  int syncing_counters;
  int iface_up;
  
  int tx_count, rx_count;

  uint32_t cur_rx_desc;
};

static struct wr_minic minic;

static inline void minic_writel(uint32_t reg,uint32_t data)
{
  *(volatile uint32_t *) (BASE_MINIC + reg) = data;
}

static inline uint32_t minic_readl(uint32_t reg)
{
  return *(volatile uint32_t *)(BASE_MINIC + reg);
}

static inline void ep_writel(uint32_t reg,uint32_t data)
{
  *(volatile uint32_t *) (BASE_EP + reg) = data;
}


static void minic_new_tx_buffer()
{
  minic.tx_head = minic.tx_base;
  minic.tx_avail = (minic.tx_size - MINIC_MTU) >> 2;
	
  minic_writel(MINIC_REG_TX_ADDR, (uint32_t) minic.tx_base);
}

void minic_init()

{
  minic_writel(MINIC_REG_EIC_IDR, MINIC_EIC_IDR_RX);
  minic_writel(MINIC_REG_EIC_ISR, MINIC_EIC_ISR_RX);
  minic.tx_base = dma_tx_buf;
  minic.tx_size = sizeof(dma_tx_buf);

	minic.tx_count = 0;

        ep_writel(EP_REG_ECR, EP_ECR_TX_EN | EP_ECR_RX_EN);
        ep_writel(EP_REG_RFCR, EP_RFCR_MRU_W(1500));
        ep_writel(EP_REG_VCR0, EP_VCR0_QMODE_W(3));
        ep_writel(EP_REG_TSCR, EP_TSCR_EN_RXTS);	

}


void minic_tx_frame(uint8_t *payload, uint32_t size)
{
  int i;
  uint32_t d_hdr, mcr, nwords, orig_size;
  
  minic_new_tx_buffer();

  orig_size = size;
  if(size < 60) size  = 60;
  if(size & 1) size  = size + 1;

  memset(minic.tx_head, 0, size + 6);
  memcpy(minic.tx_head + 1, payload, orig_size);
  memset(minic.tx_head + 1 + ((size+3)>>2), 0, 4);
    
  size /= 2;
  *minic.tx_head = TX_DESC_HAS_OWN_MAC | TX_DESC_VALID | size;
      
  minic_writel(MINIC_REG_MCR, minic_readl(MINIC_REG_MCR) | MINIC_MCR_TX_START);

  while ((minic_readl(MINIC_REG_MCR) & MINIC_MCR_TX_IDLE) == 0);
}
