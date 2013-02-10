/** @file load-search.cpp
 *  @brief Load the search table
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Grab the contents of the search component of the condition table.
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

status_t ECA::loadSearch(Device dev, bool active, std::vector<SearchEntry>& table) {
  Cycle cycle;
  eb_status_t status;
  eb_data_t   index,  first;
  eb_data_t   event1, event0;
  int done;
  
  if (!inspect_table) {
    table.clear();
    return EB_OK;
  }
  
  table.resize(table_size*2); /* Two entries for every table entry */
  
  for (unsigned i = 0; i < table.size(); ++i) {
    SearchEntry& se = table[i];
    
    if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
      return status;
    
    index = (active?0x80000000UL:0) + i;
    cycle.write(address + ECA_SEARCH, EB_DATA32, index);
    cycle.read (address + ECA_FIRST,  EB_DATA32, &first);
    cycle.read (address + ECA_EVENT1, EB_DATA32, &event1);
    cycle.read (address + ECA_EVENT0, EB_DATA32, &event0);
    cycle.close();
    
    done = 0;
    dev.flush();
    while (!done) dev.socket().run();
    if (done < 0) return done;
    if (done == 2) return EB_FAIL;
    
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

}