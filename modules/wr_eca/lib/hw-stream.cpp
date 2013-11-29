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

status_t EventStream::send(EventEntry e) {
  Cycle cycle;
  eb_status_t status;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.write(address, EB_DATA32, e.event >> 32);
  cycle.write(address, EB_DATA32, e.event & UINT32_C(0xFFFFFFFF));
  cycle.write(address, EB_DATA32, e.param >> 32);
  cycle.write(address, EB_DATA32, e.param & UINT32_C(0xFFFFFFFF));
  cycle.write(address, EB_DATA32, 0); // reserved
  cycle.write(address, EB_DATA32, e.tef   & UINT32_C(0xFFFFFFFF));
  cycle.write(address, EB_DATA32, e.time  >> 32);
  cycle.write(address, EB_DATA32, e.time  & UINT32_C(0xFFFFFFFF));
  
  return cycle.close();
}

}
