/** @file load-table.cpp
 *  @brief Load the condition table
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Grab the contents of the condition table from the ECA.
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

static status_t loadSearch(ECA* eca, bool active, std::vector<SearchEntry>& table) {
  Cycle cycle;
  eb_address_t address;
  eb_status_t status;
  eb_data_t   index,  first;
  eb_data_t   event1, event0;
  
  if (!eca->inspect_table) {
    table.clear();
    return EB_OK;
  }
  
  table.resize(eca->table_size*2); /* Two entries for every table entry */
  
  address = eca->address;
  for (unsigned i = 0; i < table.size(); ++i) {
    SearchEntry& se = table[i];
    
    if ((status = cycle.open(eca->device)) != EB_OK)
      return status;
    
    index = (active?0x80000000UL:0) + i;
    cycle.write(address + ECA_SEARCH, EB_DATA32, index);
    cycle.read (address + ECA_FIRST,  EB_DATA32, &first);
    cycle.read (address + ECA_EVENT1, EB_DATA32, &event1);
    cycle.read (address + ECA_EVENT0, EB_DATA32, &event0);
    
    if ((status = cycle.close()) != EB_OK)
      return status;
    
    se.event = event1;
    se.event <<= 32;
    se.event |= event0;
    if ((first >> 31) != 0) {
      se.first = first & 0x7FFF;
    } else {
      se.first = -1;
    }
  }
  
  return EB_OK;
}

static status_t loadWalk(ECA* eca, bool active, std::vector<WalkEntry>& table) {
  Cycle cycle;
  eb_address_t address;
  eb_status_t status;
  eb_data_t   index,  next;
  eb_data_t   delay1, delay0;
  eb_data_t   tag,    channel;
  
  if (!eca->inspect_table) {
    table.clear();
    return EB_OK;
  }
  
  table.resize(eca->table_size);
  
  address = eca->address;
  for (unsigned i = 0; i < table.size(); ++i) {
    WalkEntry& we = table[i];
    
    if ((status = cycle.open(eca->device)) != EB_OK)
      return status;
    
    index = (active?0x80000000UL:0) + i;
    cycle.write(address + ECA_WALK,   EB_DATA32, index);
    cycle.read (address + ECA_NEXT,   EB_DATA32, &next);
    cycle.read (address + ECA_DELAY1, EB_DATA32, &delay1);
    cycle.read (address + ECA_DELAY0, EB_DATA32, &delay0);
    cycle.read (address + ECA_TAG,    EB_DATA32, &tag);
    cycle.read (address + ECA_CHANNEL,EB_DATA32, &channel);
    
    if ((status = cycle.close()) != EB_OK)
      return status;
    
    we.offset = delay1;
    we.offset <<= 32;
    we.offset |= delay0;
    we.tag = tag;
    we.channel = channel;
    
    if ((next >> 31) != 0) {
      we.next = next & 0x7FFF;
    } else {
      we.next = -1;
    }
  }
  
  return EB_OK;
}

status_t ECA::load(bool active, Table& table) {
  status_t status;
  std::vector<SearchEntry> se;
  std::vector<WalkEntry> we;
  
  if ((status = loadSearch(this, active, se)) != EB_OK)
    return status;
  
  if ((status = loadWalk(this, active, we)) != EB_OK)
    return status;
  
  if (table.impl->decompile(se, we) > 0)
    return EB_FAIL;
  
  return EB_OK;
}

}
