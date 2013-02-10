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
#include "eca.h"
#include "hw-eca.h"

namespace GSI_ECA {

status_t ActionChannel::refresh(Device dev) {
  Cycle cycle;
  eb_status_t status;
  eb_data_t ctl;
  eb_data_t fill;
  int done;
  
  if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
    return status;
  
  cycle.read(address + ECAQ_CTL,  EB_DATA32, &ctl);
  cycle.read(address + ECAQ_FILL, EB_DATA32, &fill);
  cycle.close();
  
  done = 0;
  dev.flush();
  while (!done) dev.socket().run();
  if (done < 0) return done;
  if (done == 2) return EB_FAIL;
  
  draining = ((ctl >> 24) & 0x01) != 0;
  frozen   = ((ctl >> 24) & 0x02) != 0;
  fill     = (fill >> 16) & 0xFFFF;
  max_fill = (fill >>  0) & 0xFFFF;
  
  return EB_OK;
}

status_t ActionChannel::freeze(Device dev, bool freeze) {
  Cycle cycle;
  eb_status_t status;
  eb_data_t ctl;
  int done;
  
  if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
    return status;
  
  ctl = (draining?0x1:0) | (freeze?0x2:0);
  cycle.write(address + ECAQ_CTL, EB_DATA8|EB_BIG_ENDIAN, ctl);
  cycle.close();
  
  done = 0;
  dev.flush();
  while (!done) dev.socket().run();
  if (done < 0) return done;
  if (done == 2) return EB_FAIL;
  
  frozen = freeze;
  return EB_OK;
}

status_t ActionChannel::drain(Device dev, bool drain) {
  Cycle cycle;
  eb_status_t status;
  eb_data_t ctl;
  int done;
  
  if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
    return status;
  
  ctl = (drain?0x1:0) | (frozen?0x2:0);
  cycle.write(address + ECAQ_CTL, EB_DATA8|EB_BIG_ENDIAN, ctl);
  cycle.close();
  
  done = 0;
  dev.flush();
  while (!done) dev.socket().run();
  if (done < 0) return done;
  if (done == 2) return EB_FAIL;
  
  draining = drain;
  return EB_OK;
}

status_t ActionChannel::reset(Device dev) {
  Cycle cycle;
  eb_status_t status;
  int done;
  
  if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
    return status;
  
  cycle.write(address + ECAQ_MAX_FILL, EB_DATA16|EB_BIG_ENDIAN, 0);
  cycle.close();
  
  done = 0;
  dev.flush();
  while (!done) dev.socket().run();
  if (done < 0) return done;
  if (done == 2) return EB_FAIL;
  
  return EB_OK;
}

}
