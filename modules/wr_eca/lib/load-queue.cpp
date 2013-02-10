/** @file load-queue.cpp
 *  @brief Load the contents of a queue
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Retrieve the actions queued in a channel.
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

static bool sort_time(ActionEntry a, ActionEntry b) {
  return (a.time  < b.time);
}

status_t ECA::loadQueue(Device dev, unsigned channel, std::vector<ActionEntry>& table) {
  Cycle cycle;
  eb_status_t  status;
  eb_address_t base;
  eb_data_t    ctl;
  eb_data_t    time1,  time0;
  eb_data_t    event1, event0;
  eb_data_t    tag,    param;
  int done;
  
  table.clear();
  
  /* Can only probe if inspect_queue is true */
  if (!inspect_queue) return EB_OK;
  /* If the queue is not frozen, it won't work */
  if (channel >= channels.size() || !channels[channel].frozen) return EB_FAIL;
  
  base = channels[channel].address;
  
  for (unsigned i = 0; i < queue_size; ++i) {
    if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
      return status;
    
    cycle.write(base + ECAQ_INDEX,  EB_DATA16|EB_BIG_ENDIAN, i);
    cycle.read (base + ECAQ_CTL,    EB_DATA32, &ctl);
    cycle.read (base + ECAQ_TIME1,  EB_DATA32, &time1);
    cycle.read (base + ECAQ_TIME0,  EB_DATA32, &time0);
    cycle.read (base + ECAQ_EVENT1, EB_DATA32, &event1);
    cycle.read (base + ECAQ_EVENT0, EB_DATA32, &event0);
    cycle.read (base + ECAQ_TAG,    EB_DATA32, &tag);
    cycle.read (base + ECAQ_PARAM,  EB_DATA32, &param);
    cycle.close();
    
    done = 0;
    dev.flush();
    while (!done) dev.socket().run();
    if (done < 0) return done;
    if (done == 2) return EB_FAIL;
    
    /* Is the record invalid? */
    if (((ctl >> 24) & 0x80) == 0) continue;
    
    ActionEntry ae;
    
    ae.time  = time1;  ae.time  <<= 32; ae.time  += time0;
    ae.event = event1; ae.event <<= 32; ae.event += event0;
    ae.tag   = tag;
    ae.param = param;
    
    table.push_back(ae);
  }
  
  std::sort(table.begin(), table.end(), sort_time);
  
  return EB_OK;
}

}
