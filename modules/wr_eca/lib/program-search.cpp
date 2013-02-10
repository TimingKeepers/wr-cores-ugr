/** @file program-search.cpp
 *  @brief Program the search table
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Write the contents of the search component of the condition table.
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

status_t ECA::programSearch(Device dev, const std::vector<SearchEntry>& table) {
  Cycle cycle;
  eb_status_t status;
  eb_data_t first;
  int done;
  
  /* Must fit inside this hardware */
  if (table.size() > 2*table_size) return EB_OOM;
  
  /* Must have a 0 as first entry */
  if (table.empty() || table[0].event != 0) return EB_FAIL;
  
  Event last_event = 0;
  
  for (unsigned i = 0; i < 2*table_size; ++i) {
    /* Duplicate the last entry to fill out the table */
    const SearchEntry& se = (i<table.size())?table[i]:(*--table.end());
    
    /* Ensure we don't program bullshit. Must be sorted. */
    assert (last_event <= se.event);
    last_event = se.event;
    
    if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
      return status;
    
    first = (se.first==-1)?0:(UINT32_C(0x80000000)|se.first);
    
    cycle.write(address + ECA_SEARCH, EB_DATA32, i);
    cycle.write(address + ECA_FIRST,  EB_DATA32, first);
    cycle.write(address + ECA_EVENT1, EB_DATA32, se.event >> 32);
    cycle.write(address + ECA_EVENT0, EB_DATA32, se.event & UINT32_C(0xFFFFFFFF));
    cycle.close();
    
    done = 0;
    dev.flush();
    while (!done) dev.socket().run();
    if (done < 0) return done;
    if (done == 2) return EB_FAIL;
  }
  
  return EB_OK;
}

}
