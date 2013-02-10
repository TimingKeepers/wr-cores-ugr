/** @file load-walk.cpp
 *  @brief Load the walk table
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Grab the contents of the walker component of the condition table.
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

status_t ECA::loadWalk(Device dev, bool active, std::vector<WalkEntry>& table) {
  Cycle cycle;
  eb_status_t status;
  eb_data_t   index,  next;
  eb_data_t   delay1, delay0;
  eb_data_t   tag,    channel;
  int done;
  
  if (!inspect_table) {
    table.clear();
    return EB_OK;
  }
  
  table.resize(table_size);
  
  for (unsigned i = 0; i < table.size(); ++i) {
    WalkEntry& we = table[i];
    
    if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
      return status;
    
    index = (active?0x80000000UL:0) + i;
    cycle.write(address + ECA_WALK,   EB_DATA32, index);
    cycle.read (address + ECA_NEXT,   EB_DATA32, &next);
    cycle.read (address + ECA_DELAY1, EB_DATA32, &delay1);
    cycle.read (address + ECA_DELAY0, EB_DATA32, &delay0);
    cycle.read (address + ECA_TAG,    EB_DATA32, &tag);
    cycle.read (address + ECA_CHANNEL,EB_DATA32, &channel);
    cycle.close();
    
    done = 0;
    dev.flush();
    while (!done) dev.socket().run();
    if (done < 0) return done;
    if (done == 2) return EB_FAIL;
    
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

}
