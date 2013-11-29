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
  eb_data_t ctl;
  eb_data_t d_dest;
  eb_data_t d_fill;
  eb_data_t d_valid;
  eb_data_t d_conflict;
  eb_data_t d_late;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.write(address + ECAQ_SELECT, EB_DATA32, index << 16);
  cycle.read(address + ECAQ_CTL,      EB_DATA32, &ctl);
  cycle.read(address + ECAQ_INT_DEST, EB_DATA32, &d_dest);
  cycle.read(address + ECAQ_FILL,     EB_DATA32, &d_fill);
  cycle.read(address + ECAQ_VALID,    EB_DATA32, &d_valid);
  cycle.read(address + ECAQ_CONFLICT, EB_DATA32, &d_conflict);
  cycle.read(address + ECAQ_LATE,     EB_DATA32, &d_late);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  draining   = (ctl & ECAQ_CTL_DRAIN)    != 0;
  frozen     = (ctl & ECAQ_CTL_FREEZE)   != 0;
  int_enable = (ctl & ECAQ_CTL_INT_MASK) != 0;
  int_dest   = d_dest;
  fill       = (d_fill >> 16) & 0xFFFF;
  max_fill   = (d_fill >>  0) & 0xFFFF;
  valid      = d_valid    & 0xFFFFFFFF;
  conflict   = d_conflict & 0xFFFFFFFF;
  late       = d_late     & 0xFFFFFFFF;
  
  return EB_OK;
}

status_t ActionChannel::reset() {
  Cycle cycle;
  eb_status_t status;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.write(address + ECAQ_SELECT,   EB_DATA32|EB_BIG_ENDIAN, index << 16);
  cycle.write(address + ECAQ_FILL,     EB_DATA32|EB_BIG_ENDIAN, 0);
  cycle.write(address + ECAQ_VALID,    EB_DATA32|EB_BIG_ENDIAN, 0);
  cycle.write(address + ECAQ_CONFLICT, EB_DATA32|EB_BIG_ENDIAN, 0);
  cycle.write(address + ECAQ_LATE,     EB_DATA32|EB_BIG_ENDIAN, 0);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  max_fill = 0;
  valid    = 0;
  conflict = 0;
  late     = 0;
  return EB_OK;
}

status_t ActionChannel::freeze(bool freeze) {
  Cycle cycle;
  eb_status_t status;
  eb_data_t ctl;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  ctl = freeze?(ECAQ_CTL_FREEZE):(ECAQ_CTL_FREEZE<<8);
  cycle.write(address + ECAQ_SELECT, EB_DATA32|EB_BIG_ENDIAN, index << 16);
  cycle.write(address + ECAQ_CTL,    EB_DATA32|EB_BIG_ENDIAN, ctl);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  frozen = freeze;
  return EB_OK;
}

status_t ActionChannel::drain(bool drain) {
  Cycle cycle;
  eb_status_t status;
  eb_data_t ctl;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  ctl = drain?(ECAQ_CTL_DRAIN):(ECAQ_CTL_DRAIN<<8);
  cycle.write(address + ECAQ_SELECT, EB_DATA32|EB_BIG_ENDIAN, index << 16);
  cycle.write(address + ECAQ_CTL,    EB_DATA32|EB_BIG_ENDIAN, ctl);
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  draining = drain;
  return EB_OK;
}

status_t ActionChannel::hook(bool enable, uint32_t dest) {
  Cycle cycle;
  eb_status_t status;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  /* Clear the interrupt, set the address, possibly re-enable interrupt */
  cycle.write(address + ECAQ_SELECT,   EB_DATA32|EB_BIG_ENDIAN, index << 16);
  cycle.write(address + ECAQ_CTL,      EB_DATA32|EB_BIG_ENDIAN, ECAQ_CTL_INT_MASK<<8);
  cycle.write(address + ECAQ_INT_DEST, EB_DATA32|EB_BIG_ENDIAN, dest);
  if (enable) {
    cycle.write(address + ECAQ_CTL,    EB_DATA32|EB_BIG_ENDIAN, ECAQ_CTL_INT_MASK);
  }
  
  if ((status = cycle.close()) != EB_OK)
    return status;
  
  int_enable = enable;
  int_dest   = dest;
  
  return EB_OK;
}

static bool sort_time(ActionEntry a, ActionEntry b) {
  return (a.time  < b.time);
}

status_t ActionChannel::load(std::vector<ActionEntry>& table) {
  Cycle cycle;
  eb_status_t  status;
  eb_data_t    ctl;
  eb_data_t    event1, event0;
  eb_data_t    param1, param0;
  eb_data_t    tag,    tef;
  eb_data_t    time1,  time0;
  
  table.clear();
  
  /* If the queue is not frozen, it won't work */
  if (!frozen) return EB_FAIL;
  
  for (unsigned i = 0; i < queue_size; ++i) {
    if ((status = cycle.open(device)) != EB_OK)
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
    if (((ctl >> 24) & ECAQ_STATUS_VALID) == 0) continue;
    
    ActionEntry ae;
    
    ae.event = event1; ae.event <<= 32; ae.event += event0;
    ae.param = param1; ae.param <<= 32; ae.param += param0;
    ae.tag   = tag;
    ae.tef   = tef;
    ae.time  = time1;  ae.time  <<= 32; ae.time  += time0;
    
    /* late events have negative timestamps */
    if (((ctl >> 24) & ECAQ_STATUS_LATE) == 1) ae.time = -ae.time;
    
    table.push_back(ae);
  }
  
  std::sort(table.begin(), table.end(), sort_time);
  
  return EB_OK;
}

}
