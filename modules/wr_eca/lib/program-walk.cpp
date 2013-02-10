/** @file program-walk.cpp
 *  @brief Program the walk table
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Write the contents of the walker component of the condition table.
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

status_t ECA::programWalk(Device dev, const std::vector<WalkEntry>& table) {
  Cycle cycle;
  eb_status_t status;
  eb_data_t next;
  int done;
  
  /* Must fit inside this hardware */
  if (table.size() > table_size) return EB_OOM;
  
  for (unsigned i = 0; i < table.size(); ++i) {
    const WalkEntry& we = table[i];
    
    /* Ensure we don't program bullshit */
    assert (we.channel < channels.size());
    assert (we.next < (int)i);
    
    if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
      return status;
    
    next = (we.next==-1)?0:(UINT32_C(0x80000000)|we.next);
    
    cycle.write(address + ECA_WALK,    EB_DATA32, i);
    cycle.write(address + ECA_NEXT,    EB_DATA32, next);
    cycle.write(address + ECA_DELAY1,  EB_DATA32, we.offset >> 32);
    cycle.write(address + ECA_DELAY0,  EB_DATA32, we.offset & UINT32_C(0xFFFFFFFF));
    cycle.write(address + ECA_TAG,     EB_DATA32, we.tag);
    cycle.write(address + ECA_CHANNEL, EB_DATA32, we.channel);
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
