/** @file hw-eca.cpp
 *  @brief C++ Wrapper for the ECA Unit hardware.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Read-write control registers of ECA unit.
 *  Convenience functions for dealing with ECA constants.
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
#include <time.h>
#include "eca.h"
#include "hw-eca.h"

namespace GSI_ECA {

std::string ECA::frequency(void) const {
  double freq = freq_mul;
  freq /= freq_div;
  
  for (unsigned i = 0; i < freq_5s; ++i) freq *= 5;
  for (unsigned i = 0; i < freq_2s; ++i) freq *= 2;
  
  char output[40];
  if (freq >= 1e27) {
    snprintf(output, sizeof(output), "%.6eHz", freq);
  } else if (freq >= 1e24) {
    snprintf(output, sizeof(output), "%.3fYHz", freq/1e24);
  } else if (freq >= 1e21) {
    snprintf(output, sizeof(output), "%.3fZHz", freq/1e21);
  } else if (freq >= 1e18) {
    snprintf(output, sizeof(output), "%.3fEHz", freq/1e18);
  } else if (freq >= 1e15) {
    snprintf(output, sizeof(output), "%.3fPHz", freq/1e15);
  } else if (freq >= 1e12) {
    snprintf(output, sizeof(output), "%.3fTHz", freq/1e12);
  } else if (freq >= 1e9) {
    snprintf(output, sizeof(output), "%.3fGHz", freq/1e9);
  } else if (freq >= 1e6) {
    snprintf(output, sizeof(output), "%.3fMHz", freq/1e6);
  } else if (freq >= 1e3) {
    snprintf(output, sizeof(output), "%.3fkHz", freq/1e3);
  } else {
    snprintf(output, sizeof(output), "%.3fHz", freq);
  }
  
  return output;
}

void ECA::seconds(uint64_t* seconds, double* subseconds, uint64_t t) const {
  /* desire: time/freq */
  if (t == (uint64_t)-1) t = time;
  
  /* Have to be very careful about rounding here ... */
  
  /* Step0: split the time into two parts */
  uint64_t high = t >> 32;           /* bits [32,63) */
  uint64_t low  = t & 0xFFFFFFFFUL;  /* bits [ 0,32) */
  double   rem = 0;
  double   div = 1;
  
  /* Step1: multply by freq_div (16 bits) */
  high *= freq_div;
  low  *= freq_div;
  
  high += (low >> 32);  /* bits [32,80) */
  low  &= 0xFFFFFFFFUL; /* bits [ 0,32) */
  
  /* Step2: divide by 2 repeatedly, rounding down */
  for (unsigned i = 0; i < freq_2s; ++i) {
    low += (high%2)<<32;
    rem += (low%2)*div;
    div *= 2;
    low  /= 2;
    high /= 2;
  }
  
  /* Divide by 5 repeatedly, rounding down */
  for (unsigned i = 0; i < freq_5s; ++i) {
    low += (high%5)<<32;
    rem += (low%5)*div;
    div *= 5;
    low  /= 5;
    high /= 5;
  }
  
  /* Divide by freq_mul (32-bits), rounding down */
  low += (high%freq_mul)<<32;
  rem += (low%freq_mul)*div;
  div *= freq_mul;
  low  /= freq_mul;
  high /= freq_mul;
  
  /* Recombine word */
  low += high << 32;
  high >>= 32;
  
  /* Can only overflow if frequency was slower than 1Hz */
  assert(high == 0);
  
  *seconds = low;
  *subseconds = rem/div;
}

std::string ECA::date(uint64_t t) const {
  char buf[40];
  uint64_t s;
  double subs;
  struct tm *tm;
  time_t ctime;
  
  seconds(&s, &subs, t);
  
  snprintf(buf, sizeof(buf), "%.9f", subs);
  std::string tail(&buf[1]);
  
  ctime = s;
  tm = gmtime(&ctime);
  strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", tm);
  
  return std::string(buf) + tail;
}

uint64_t ECA::delay(double seconds) const {
  double out = seconds;
  
  out *= freq_mul;
  out /= freq_div;
  
  for (unsigned i = 0; i < freq_5s; ++i) out *= 5;
  for (unsigned i = 0; i < freq_2s; ++i) out *= 2;
  
  return out;
}

double ECA::delay(uint64_t seconds) const {
  double out = (int64_t)seconds;
  
  out /= freq_mul;
  out *= freq_div;
  
  for (unsigned i = 0; i < freq_5s; ++i) out /= 5;
  for (unsigned i = 0; i < freq_2s; ++i) out /= 2;
  
  return out;
}

status_t ECA::refresh() {
  Cycle cycle;
  eb_status_t status;
  eb_data_t control;
  eb_data_t time1, time0;
  
  if ((status = cycle.open(device)) != EB_OK)
    return status;
  
  cycle.read(address + ECA_CTL,   EB_DATA32, &control);
  cycle.read(address + ECA_TIME1, EB_DATA32, &time1);
  cycle.read(address + ECA_TIME0, EB_DATA32, &time0);
  
  if ((status = cycle.close()) != EB_OK) return status;
  
  time = time1;
  time <<= 32;
  time += time0;
  disabled   = (control & ECA_CTL_DISABLE)    != 0;
  interrupts = (control & ECA_CTL_INT_ENABLE) != 0;
  
  return EB_OK;
}

status_t ECA::disable(bool d) {
  eb_status_t status;
  eb_data_t ctl;
  
  ctl = d?(ECA_CTL_DISABLE):(ECA_CTL_DISABLE<<8);
  status = device.write(address + ECA_CTL, EB_DATA32|EB_BIG_ENDIAN, ctl);
  if (status != EB_OK) return status;
  
  disabled = d;
  
  return EB_OK;
}

status_t ECA::interrupt(bool i) {
  eb_status_t status;
  eb_data_t ctl;
  
  ctl = i?(ECA_CTL_INT_ENABLE):(ECA_CTL_INT_ENABLE<<8);
  status = device.write(address + ECA_CTL, EB_DATA32|EB_BIG_ENDIAN, ctl);
  if (status != EB_OK) return status;
  
  interrupts = i;
  
  return EB_OK;
  
}

status_t ECA::flipTables() {
  return device.write(address + ECA_CTL, EB_DATA32|EB_BIG_ENDIAN, ECA_CTL_FLIP);
}

}
