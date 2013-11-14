/** @file hw-channel.cpp
 *  @brief C++ Wrapper for the ECA Channel hardware.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Read-write control of Channel registers.
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

#define __STDC_FORMAT_MACROS
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#include <stdio.h>
#include <assert.h>
#include <algorithm>
#include "eca.h"
#include "hw-eca.h"

namespace GSI_ECA {

status_t ActionChannel::refresh() {
  Cycle cycle;
  eb_status_t status;
  eb_address_t address;
  eb_data_t ctl;
  eb_data_t d_fill;
  eb_data_t d_valid;
  eb_data_t d_late;
  
  if ((status = cycle.open(eca->device)) != EB_OK)
    return status;
  
  address = eca->address;
  cycle.write(address + ECAQ_SELECT, EB_DATA32, index << 16);
  cycle.read(address + ECAQ_CTL,   EB_DATA32, &ctl);
  cycle.read(address + ECAQ_FILL,  EB_DATA32, &d_fill);
  cycle.read(address + ECAQ_VALID, EB_DATA32, &d_valid);
  cycle.read(address + ECAQ_LATE,  EB_DATA32, &d_late);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  draining = ((ctl >> 24) & 0x01) != 0;
  frozen   = ((ctl >> 24) & 0x02) != 0;
  fill     = (d_fill >> 16) & 0xFFFF;
  max_fill = (d_fill >>  0) & 0xFFFF;
  valid    = d_valid & 0xFFFFFFFF;
  late     = d_late  & 0xFFFFFFFF;
  
  return EB_OK;
}

status_t ActionChannel::freeze(bool freeze) {
  Cycle cycle;
  eb_status_t status;
  eb_address_t address;
  eb_data_t ctl;
  
  if ((status = cycle.open(eca->device)) != EB_OK)
    return status;
  
  address = eca->address;
  ctl = (draining?0x1:0) | (freeze?0x2:0);
  cycle.write(address + ECAQ_SELECT, EB_DATA16|EB_BIG_ENDIAN, index);
  cycle.write(address + ECAQ_CTL, EB_DATA8|EB_BIG_ENDIAN, ctl);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  frozen = freeze;
  return EB_OK;
}

status_t ActionChannel::drain(bool drain) {
  Cycle cycle;
  eb_address_t address;
  eb_status_t status;
  eb_data_t ctl;
  
  if ((status = cycle.open(eca->device)) != EB_OK)
    return status;
  
  address = eca->address;
  ctl = (drain?0x1:0) | (frozen?0x2:0);
  cycle.write(address + ECAQ_SELECT, EB_DATA16|EB_BIG_ENDIAN, index);
  cycle.write(address + ECAQ_CTL, EB_DATA8|EB_BIG_ENDIAN, ctl);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  draining = drain;
  return EB_OK;
}

status_t ActionChannel::reset() {
  Cycle cycle;
  eb_address_t address;
  eb_status_t status;
  
  if ((status = cycle.open(eca->device)) != EB_OK)
    return status;
  
  address = eca->address;
  cycle.write(address + ECAQ_SELECT, EB_DATA16|EB_BIG_ENDIAN, index);
  cycle.write(address + ECAQ_FILL,  EB_DATA32|EB_BIG_ENDIAN, 0);
  cycle.write(address + ECAQ_VALID, EB_DATA32|EB_BIG_ENDIAN, 0);
  cycle.write(address + ECAQ_LATE,  EB_DATA32|EB_BIG_ENDIAN, 0);
  return cycle.close();
}

static bool sort_time(ActionEntry a, ActionEntry b) {
  return (a.time  < b.time);
}

status_t ActionChannel::load(std::vector<ActionEntry>& table) {
  Cycle cycle;
  eb_address_t address;
  eb_status_t  status;
  eb_data_t    ctl;
  eb_data_t    event1, event0;
  eb_data_t    param1, param0;
  eb_data_t    tag,    tef;
  eb_data_t    time1,  time0;
  
  table.clear();
  
  /* Can only probe if inspect_queue is true */
  if (!eca->inspect_queue) return EB_OK;
  
  /* If the queue is not frozen, it won't work */
  if (!frozen) return EB_FAIL;
  
  address = eca->address;
  for (unsigned i = 0; i < eca->queue_size; ++i) {
    if ((status = cycle.open(eca->device)) != EB_OK)
      return status;
    
    cycle.write(address + ECAQ_SELECT, EB_DATA32|EB_BIG_ENDIAN, (index << 16) | i);
    cycle.read (address + ECAQ_CTL,    EB_DATA32, &ctl);
    
    cycle.read (address + ECAQ_EVENT1, EB_DATA32, &event1);
    cycle.read (address + ECAQ_EVENT0, EB_DATA32, &event0);
    cycle.read (address + ECAQ_PARAM1, EB_DATA32, &param1);
    cycle.read (address + ECAQ_PARAM0, EB_DATA32, &param0);
    cycle.read (address + ECAQ_TAG,    EB_DATA32, &tag);
    cycle.read (address + ECAQ_TEF,    EB_DATA32, &tef);
    cycle.read (address + ECAQ_TIME1,  EB_DATA32, &time1);
    cycle.read (address + ECAQ_TIME0,  EB_DATA32, &time0);
    
    if ((status = cycle.close()) != EB_OK)
      return status;
    
    /* Is the record invalid? */
    if (((ctl >> 24) & 0x80) == 0) continue;
    
    ActionEntry ae;
    
    ae.event = event1; ae.event <<= 32; ae.event += event0;
    ae.param = param1; ae.param <<= 32; ae.param += param0;
    ae.tag   = tag;
    ae.tef   = tef;
    ae.time  = time1;  ae.time  <<= 32; ae.time  += time0;
    
    table.push_back(ae);
  }
  
  std::sort(table.begin(), table.end(), sort_time);
  
  return EB_OK;
}

}
