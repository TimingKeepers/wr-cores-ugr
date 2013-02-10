/** @file hw-stream.cpp
 *  @brief C++ Wrapper for the ECA Stream hardware.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Send test events to the ECA unit.
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

status_t EventStream::send(Device dev, Event event, Time time, Param param) {
  Cycle cycle;
  eb_status_t status;
  int done;
  
  if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
    return status;
  
  cycle.write(address, EB_DATA32, event >> 32);
  cycle.write(address, EB_DATA32, event & UINT32_C(0xFFFFFFFF));
  cycle.write(address, EB_DATA32, time >> 32);
  cycle.write(address, EB_DATA32, time & UINT32_C(0xFFFFFFFF));
  cycle.write(address, EB_DATA32, param);
  cycle.close();
  
  done = 0;
  dev.flush();
  while (!done) dev.socket().run();
  if (done < 0) return done;
  if (done == 2) return EB_FAIL;
  
  return EB_OK;
}

}
