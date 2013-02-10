/** @file load-eca.cpp
 *  @brief Locate ECA units on an Etherbone device.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Find all ECA units on device.
 *  Load contents of all pertinent registers.
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

struct SearchRecord {
  int done;
  eb_status_t status;
  std::vector<ECA>* ecas;
  std::vector<EventStream> streams;
};

static void trim(std::string& s) {
  std::string::size_type x = s.size();
  for (x = s.size(); x > 0; --x)
    if (s[x-1] != ' ') break;
  s.resize(x);
}

void eca_sdb_search(SearchRecord* record, Device dev, sdb_t sdb, status_t status) {
  if (status != EB_OK) {
    record->status = status;
    record->done = 1;
    return;
  }
  
  unsigned devices = sdb->interconnect.sdb_records - 1;
  for (unsigned i = 0; i < devices; ++i) {
    sdb_record_t des = &sdb->record[i];
    
    switch (des->empty.record_type) {
      case sdb_device: {
        if (des->device.sdb_component.product.vendor_id == GSI_VENDOR_ID) {
          switch (des->device.sdb_component.product.device_id) {
            case ECAE_DEVICE_ID: {
              EventStream es;
              es.address       = des->device.sdb_component.addr_first;
              es.sdb_ver_major = des->device.abi_ver_major;
              es.sdb_ver_minor = des->device.abi_ver_minor;
              es.sdb_version   = des->device.sdb_component.product.version;
              es.sdb_date      = des->device.sdb_component.product.date;
              es.sdb_name      = std::string((const char*)&des->device.sdb_component.product.name[0], 19);
              trim(es.sdb_name);
              record->streams.push_back(es);
              break;
            }
            case ECA_DEVICE_ID: {
              ECA eca;
              eca.address       = des->device.sdb_component.addr_first;
              eca.sdb_ver_major = des->device.abi_ver_major;
              eca.sdb_ver_minor = des->device.abi_ver_minor;
              eca.sdb_version   = des->device.sdb_component.product.version;
              eca.sdb_date      = des->device.sdb_component.product.date;
              eca.sdb_name      = std::string((const char*)&des->device.sdb_component.product.name[0], 19);
              trim(eca.sdb_name);
              record->ecas->push_back(eca);
              break;
            }
          }
        }
        break;
      }
      case sdb_bridge: {
        dev.sdb_scan_bus(&des->bridge, record, sdb_wrap_function_callback<SearchRecord, eca_sdb_search>);
        
        record->done = 0;
        while (!record->done) dev.socket().run();
        break;
      }
    }
  }
  
  record->done = 1;
}

void eca_cycle_done(int* done, Device dev, Operation op, eb_status_t status) {
  if (status == EB_OK) {
    *done = 1;
    for (; !op.is_null(); op = op.next()) {
      if (op.had_error()) {
        *done = 2;
        fprintf(stderr, "Wishbone segfault %s address %"EB_ADDR_FMT"\n",
                op.is_read()?"reading":"writing", op.address());
      }
    }
  } else {
    fprintf(stderr, "Etherbone error: %s\n", eb_status(status));
    *done = status;
  }
}

std::string eca_extract_name(eb_data_t* data) {
  char name8[64];
  
  /* Unshift/extract the name from the control register */
  int zero = -1;
  int nonzero = -1;
  for (unsigned j = 0; j < 64; ++j) {
    name8[j] = ((data[j] >> 16) & 0xFF);
    if (zero == -1 && nonzero == -1 && !name8[j])
      zero = j;
    if (zero != -1 && nonzero == -1 && name8[j])
      nonzero = j;
  }
  if (zero == -1) return std::string(&name8[0], 63); /* Not terminated => cannot unwrap */
  if (nonzero == -1) nonzero = 0; /* If was zeros till end, no shift needed */
  for (unsigned j = 0; j < 64; ++j) {
    name8[j] = ((data[(j+nonzero)&0x3f] >> 16) & 0xFF);
  }
    
  return std::string(&name8[0]);
}


status_t ECA::load(Device dev, std::vector<ECA>& ecas) {
  /* Phase 1 -- locate ECA units using SDB */
  SearchRecord record;
  record.ecas = &ecas;
  
  dev.sdb_scan_root(&record, sdb_wrap_function_callback<SearchRecord, eca_sdb_search>);
  
  record.done = 0;
  record.status = EB_OK;
  
  while (!record.done) dev.socket().run();
  if (record.status != EB_OK) return record.status;
  
  /* Phase 2a -- Read ECA parameters */
  eb_status_t status;
  eb_data_t name[64];
  eb_data_t sizes;
  eb_data_t time1, time0;
  eb_data_t freq1, freq0;
  eb_data_t fill;
  eb_data_t id;
  
  Cycle cycle;
  int done;
  
  for (unsigned i = 0; i < ecas.size(); ++i) {
    ECA& eca = ecas[i];
    unsigned num_channels;
    
    if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
      return status;
    
    for (unsigned j = 0; j < 64; ++j)
      cycle.read(eca.address + ECA_CTL, EB_DATA32, &name[j]);
    
    cycle.read(eca.address + ECA_INFO,     EB_DATA32, &sizes);
    cycle.read(eca.address + ECA_TIME1,    EB_DATA32, &time1);
    cycle.read(eca.address + ECA_TIME0,    EB_DATA32, &time0);
    cycle.read(eca.address + ECA_FREQ_MUL, EB_DATA32, &freq1);
    cycle.read(eca.address + ECA_FREQ_5S,  EB_DATA32, &freq0);
    cycle.write(eca.address + ECA_INDEX, EB_BIG_ENDIAN|EB_DATA8, i); /* set index for matching streams */
    cycle.close();
    
    done = 0;
    dev.flush();
    while (!done) dev.socket().run();
    if (done < 0) return done;
    if (done == 2) return EB_FAIL;
    
    eca.name = eca_extract_name(name);
    
    eca.inspect_table = ((name[0] >> 24) & 0x80) != 0;
    eca.inspect_queue = ((name[0] >> 24) & 0x40) != 0;
    eca.disabled      = ((name[0] >> 24) & 0x01) != 0;
    
    eca.table_size = 1 << ((sizes >> 24) & 0xff);
    eca.queue_size = 1 << ((sizes >> 16) & 0xff);
    num_channels   =    ((sizes >>  8) & 0xff);
    
    eca.time = time1;
    eca.time <<= 32;
    eca.time |= time0;
    
    eca.freq_mul = freq1;
    eca.freq_5s  = ((freq0 >> 24) & 0xff);
    eca.freq_2s  = ((freq0 >> 16) & 0xff);
    eca.freq_div = ((freq0 >>  0) & 0xffff);
    
    /* Phase 2b -- Read Channel parameters + names */
    for (unsigned c = 0; c < num_channels; ++c) {
      ActionChannel ac;
      ac.address = eca.address + ECA_END + c*ECAQ_END;
      
      if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
        return status;
      
      for (unsigned j = 0; j < 64; ++j)
        cycle.read(ac.address + ECAQ_CTL, EB_DATA32, &name[j]);
      cycle.read(ac.address + ECAQ_FILL, EB_DATA32, &fill);
      cycle.close();
      
      done = 0;
      dev.flush();
      while (!done) dev.socket().run();
      if (done < 0) return done;
      if (done == 2) return EB_FAIL;
      
      ac.name     = eca_extract_name(name);
      ac.draining = ((name[0] >> 24) & 0x01) != 0;
      ac.frozen   = ((name[0] >> 24) & 0x02) != 0;
      ac.fill     = (fill >> 16) & 0xFFFF;
      ac.max_fill = (fill >>  0) & 0xFFFF;
      
      eca.channels.push_back(ac);
    }
  }
  
  /* Phase 3 -- Deduce stream relationships */
  std::vector<uint8_t> ids;
  ids.resize(record.streams.size());
  
  for (unsigned s = 0; s < record.streams.size(); ++s) {
    EventStream& es = record.streams[s];
    
    if ((status = cycle.open(dev, &done, wrap_function_callback<int, eca_cycle_done>)) != EB_OK)
      return status;
      
    /* These have to be separate cycles so the crossbar doesn't get stuffed up */
    cycle.read(es.address, EB_DATA32, &id);
    cycle.close();
    
    done = 0;
    dev.flush();
    while (!done) dev.socket().run();
    if (done < 0) return done;
    if (done == 2) return EB_FAIL;
    
    ids[s] = id;
  }
  
  for (unsigned s = 0; s < record.streams.size(); ++s) {
    EventStream& es = record.streams[s];
    uint8_t mid = ids[s];
    if (mid >= ecas.size()) {
      /* fprintf(stderr, "Unmatched ECA Event stream; id: %d\n", mid); */
      continue;
    }
    ecas[mid].streams.push_back(es);
  }
  
  return EB_OK;
}

}
