/** @file store-table.cpp
 *  @brief Program the walk table
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Write the contents of the condition table to the ECA.
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
#include "eca.h"
#include "hw-eca.h"

namespace GSI_ECA {

static status_t storeSearch(ECA* eca, const std::vector<SearchEntry>& table) {
  Cycle cycle;
  unsigned table_size;
  eb_address_t address;
  eb_status_t status;
  eb_data_t first;
  
  /* Must fit inside this hardware */
  table_size = eca->table_size;
  if (table.size() > 2*table_size) return EB_OOM;
  
  /* Must have a 0 as first entry */
  if (table.empty() || table[0].event != 0) return EB_FAIL;
  
  Event last_event = 0;
  
  address = eca->address;
  for (unsigned i = 0; i < 2*table_size; ++i) {
    /* Duplicate the last entry to fill out the table */
    const SearchEntry& se = (i<table.size())?table[i]:(*--table.end());
    
    /* Ensure we don't program bullshit. Must be sorted. */
    assert (last_event <= se.event);
    last_event = se.event;
    
    if ((status = cycle.open(eca->device)) != EB_OK)
      return status;
    
    first = (se.first==-1)?0:(UINT32_C(0x80000000)|se.first);
    
    cycle.write(address + ECA_SEARCH, EB_DATA32, i);
    cycle.write(address + ECA_FIRST,  EB_DATA32, first);
    cycle.write(address + ECA_EVENT1, EB_DATA32, se.event >> 32);
    cycle.write(address + ECA_EVENT0, EB_DATA32, se.event & UINT32_C(0xFFFFFFFF));
    
    if ((status = cycle.close()) != EB_OK)
      return status;
  }
  
  return EB_OK;
}

static status_t storeWalk(ECA* eca, const std::vector<WalkEntry>& table) {
  Cycle cycle;
  eb_address_t address;
  eb_status_t status;
  eb_data_t next;
  unsigned channels;
  
  /* Must fit inside this hardware */
  channels = eca->channels.size();
  if (table.size() > eca->table_size) return EB_OOM;
  
  address = eca->address;
  for (unsigned i = 0; i < table.size(); ++i) {
    const WalkEntry& we = table[i];
    
    /* Ensure we don't program bullshit */
    if (we.channel >= channels) return EB_FAIL;
    assert (we.next < (int)i);
    
    if ((status = cycle.open(eca->device)) != EB_OK)
      return status;
    
    next = (we.next==-1)?0:(UINT32_C(0x80000000)|we.next);
    
    cycle.write(address + ECA_WALK,    EB_DATA32, i);
    cycle.write(address + ECA_NEXT,    EB_DATA32, next);
    cycle.write(address + ECA_DELAY1,  EB_DATA32, we.offset >> 32);
    cycle.write(address + ECA_DELAY0,  EB_DATA32, we.offset & UINT32_C(0xFFFFFFFF));
    cycle.write(address + ECA_TAG,     EB_DATA32, we.tag);
    cycle.write(address + ECA_CHANNEL, EB_DATA32, we.channel);
    
    if ((status = cycle.close()) != EB_OK)
      return status;
  }
  
  return EB_OK;
}

status_t ECA::store(const Table& table) {
  status_t status;
  std::vector<SearchEntry> se;
  std::vector<WalkEntry> we;
  
  table.impl->compile(se, we);
  
  if ((status = storeSearch(this, se)) != EB_OK)
    return status;
  if ((status = storeWalk(this, we)) != EB_OK)
    return status;
  
  return EB_OK;
}

}
