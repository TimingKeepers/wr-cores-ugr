/** @file hw-eca.h
 *  @brief Register layout of the ECA unit.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  All offsets for regsiters in the ECA unit.
 *
 *******************************************************************************
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 3 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *  
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library. If not, see <http://www.gnu.org/licenses/>.
 *******************************************************************************
 */

#ifndef ECA_HW_H
#define ECA_HW_H

#include <etherbone.h>

#define GSI_VENDOR_ID	0x651
#define ECA_DEVICE_ID	0x8752bf44U
#define ECAE_DEVICE_ID	0x8752bf45U

#define ECA_CTL		0x00
#define ECA_NAME	0x01
#define ECA_INFO	0x04
#define ECA_TABLE_SIZE	0x04
#define ECA_QUEUE_SIZE	0x05
#define ECA_NUM_CHANNELS 0x06
#define ECA_INDEX	0x07
#define ECA_TIME1	0x08
#define ECA_TIME0	0x0C
#define ECA_SEARCH	0x10
#define ECA_FIRST	0x14
#define ECA_EVENT1	0x18
#define ECA_EVENT0	0x1C
#define ECA_WALK	0x20
#define ECA_NEXT	0x24
#define ECA_DELAY1	0x28
#define ECA_DELAY0	0x2C
#define ECA_TAG		0x30
#define ECA_CHANNEL	0x34
#define ECA_FREQ_MUL	0x38
#define ECA_FREQ_5S	0x3C
#define ECA_FREQ_2S	0x3D
#define ECA_FREQ_DIV	0x3E
#define ECA_END		0x40

#define ECAQ_CTL	0x00
#define ECAQ_NAME	0x01
#define ECAQ_INDEX	0x02
#define ECAQ_FILL	0x04
#define ECAQ_MAX_FILL	0x06
#define ECAQ_TIME1	0x08
#define ECAQ_TIME0	0x0C
#define ECAQ_EVENT1	0x10
#define ECAQ_EVENT0	0x14
#define ECAQ_TAG	0x18
#define ECAQ_PARAM	0x1C
#define ECAQ_END	0x20

namespace GSI_ECA {
extern void eca_cycle_done(int* done, etherbone::Device dev, etherbone::Operation op, eb_status_t status);
}

#endif
