/** @file table.cpp
 *  @brief Convert to/from user-facing condition table
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Decompile search+walker tables into condition table
 *  Compile condition table to search+walker tables
 *  Modify table by adding/removing rules.
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
#include <algorithm>
#include "eca.h"
#include "hw-eca.h"

// #define DEBUG 1

namespace GSI_ECA {

Table::Table() :
  impl(new Impl) {
}

Table::~Table() {
  delete impl;
}

Table::Table(const Table& table) 
  : impl(new Impl(*table.impl)) {
}

int Table::add(const TableEntry& te) {
  return impl->add(te);
}

int Table::del(const TableEntry& te) {
  return impl->remove(te);
}

int Table::set(const std::vector<TableEntry>& vect) {
  return impl->set(vect);
}

void Table::get(std::vector<TableEntry>& vect) const {
  return impl->get(vect);
}

int Table::Impl::add(Event begin, Event end, Time offset, Channel channel, Tag tag) {
  int out = 0;
  
  if (end < begin) return out;
  
  if (channel >= data.size()) data.resize(channel+1);
  EventFilter& ef = data[channel][offset];

  /* First element i, with i.begin > end+1 */
  /* --i is thus last element with i.begin <= end+1 */
  EventFilter::iterator i = 
    (end+1 == 0)?ef.end():ef.upper_bound(end+1); 
  
  /* Deal with intersecting ranges */
  while (!ef.empty() && i != ef.begin() && (begin-1 <= (--i)->second.end || begin==0)) {
    /* i exists AND begin <= i.end+1 and i.begin <= end+1 */
    if (i->second.tag != tag) {
      if (i->first == end+1) {
        /* Don't merge these, but look for more */
        continue;
      } else if (i->second.end+1 == begin) {
        /* Don't merge these. Was also last */
        break;
      } else {
        /* Ranges have non-empty overlap and conflicting tag */
        ++out;
      }
    }
    
#ifdef DEBUG
    printf("merging %"PRIx64"-%"PRIx64"\n", i->first, i->second.end);
#endif
    
    /* Merge the range */
    if (i->first      < begin) begin = i->first;
    if (i->second.end > end)   end   = i->second.end;
    
    /* Note: invariant is that the ranges are disjoint.
     * Therefore, extending end cannot reach any larger ranges.
     */
    
    EventFilter::iterator kill = i;
    ++i;
    ef.erase(kill);
  }
  
  /* Create a new node */
  EventRange& er = ef[begin];
  er.end = end;
  er.tag = tag;
#ifdef DEBUG
  printf("storing %"PRIx64"-%"PRIx64"\n", begin, end);
#endif
  
  return out;
}

int Table::Impl::remove(Event begin, Event end, Time offset, Channel channel) {
  int out = 0;
  
  if (end < begin) return out;
  
  if (channel >= data.size()) return out;
  TableActions& ta = data[channel];
  
  TableActions::iterator efi = ta.find(offset);
  if (efi == ta.end()) return out;
  
  EventFilter& ef = efi->second;

  /* First element i, with i.begin > end */
  /* --i is thus last element with i.begin <= end */
  EventFilter::iterator i = ef.upper_bound(end); 
  
  while (!ef.empty() && i != ef.begin() && begin <= (--i)->second.end) {
    /* i exists AND begin <= i.end and i.begin <= end */
    ++out;
    
    /* Four ways the existing record can overlap */
    if (i->first < begin) {
      if (end < i->second.end) {
        /* i.begin < begin <= end < i.end */
        /* our range is STRICTLY contained in theirs => split their range */
        ef[end+1] = i->second;
        i->second.end = begin-1;
      } else {
        /* i.begin < begin <= i.end <= end */
        /* our range affects their range ending => shorten it */
        i->second.end = begin-1;
      }
    } else {
      if (end < i->second.end) {
        /* begin <= i.begin <= end < i.end */
        /* our range affects their range beginning => re-insert it */
        ef[end+1] = i->second;
        EventFilter::iterator kill = i;
        ++i;
        ef.erase(kill);
      } else {
        /* begin <= i.begin <= i.end <= end */
        /* their range is contained in ours => just delete them */
        EventFilter::iterator kill = i;
        ++i;
        ef.erase(kill);
      }
    }
  }
  
  return out;
}

static Event eca_encode_mask(uint8_t event_bits) {
  Event mask;
  
  if (event_bits > 64) {
    return 0;
  } else if (event_bits == 64) {
    mask = 0;
  } else {
    mask = ((Event)-1) >> event_bits;
  }
  
  return mask;
}  


int Table::Impl::add(const TableEntry& te) {
  Event mask  = eca_encode_mask(te.event_bits);
  Event begin = te.event & ~mask;
  Event end   = begin | mask;
  
  return add(begin, end, te.offset, te.channel, te.tag);
}

int Table::Impl::remove(const TableEntry& te) {
  Event mask  = eca_encode_mask(te.event_bits);
  Event begin = te.event & ~mask;
  Event end   = begin | mask;
  
  return remove(begin, end, te.offset, te.channel);
}

int Table::Impl::set(const std::vector<TableEntry>& t) {
  int count = 0;
  
  for (unsigned ti = 0; ti < t.size(); ++ti) {
    count += add(t[ti]);
  }
  
  return count;
}

int Table::Impl::decompile(const std::vector<SearchEntry>& s, const std::vector<WalkEntry>& w) {
  int count = 0;
  
  Event last = ((Event)-1);
  for (unsigned si = s.size(); si != 0; --si) {
    const SearchEntry& se = s[si-1];
    for (int16_t wi = se.first; wi != -1; wi = w[wi].next) {
      if (wi >= (int)w.size()) { ++count; break; }
      const WalkEntry& we = w[wi];
#ifdef DEBUG
      if (se.event < last)
        printf("HW: %"PRIx64"-%"PRIx64": %"PRIx64" %d %"PRIx32"\n", se.event, last, we.offset, we.channel, we.tag);
#endif
      count += add(se.event, last, we.offset, we.channel, we.tag);
    }
    last = se.event-1;
  }
  
  return count;
}

static bool sort_event_bits_offset_channel(const TableEntry& a, const TableEntry& b) {
  if (a.event      < b.event)      return true;
  if (a.event      > b.event)      return false;
  if (a.event_bits < b.event_bits) return true;
  if (a.event_bits > b.event_bits) return false;
  if (a.offset     < b.offset)     return true;
  if (a.offset     > b.offset)     return false;
  return a.channel < b.channel;
}

/* 0 -> 64 */
/* 1 -> 63 */
/* 3 -> 62 */
/* FF.FF -> 0 */
static unsigned eca_decode_mask(Event mask) {
  unsigned count;
  for (count = 64; mask != 0; --count)
    mask >>= 1;
  return count;
}

void Table::Impl::get(std::vector<TableEntry>& result) const {
  result.clear();
  
  for (unsigned c = 0; c < data.size(); ++c) {
    const TableActions& ta = data[c];
    TableActions::const_iterator ti;
    for (ti = ta.begin(); ti != ta.end(); ++ti) {
      const EventFilter& ef = ti->second;
      EventFilter::const_iterator ei;
      for (ei = ef.begin(); ei != ef.end(); ++ei) {
        TableEntry te;
        
        te.offset  = ti->first;
        te.tag     = ei->second.tag;
        te.channel = c;
        
        Event begin = ei->first;
        Event end   = ei->second.end;
        Event mask;

#ifdef DEBUG        
        printf("TO_USER: %"PRIx64"-%"PRIx64": %"PRIx64" %d %"PRIx32"\n", 
               begin, end, te.offset, te.channel, te.tag);
#endif
        
        while (1) {
          /* 1s in the places of begin's trailing 0s */
          mask = (begin^(begin-1)) & ~begin;
          
          if ((begin|mask) >= end) break;
          
          te.event = begin;
          te.event_bits = eca_decode_mask(mask);
          result.push_back(te);
          
          begin |= mask;
          ++begin;
        }
        
        while (1) {
          /* 1s in the places of end's trailing 1s */
          mask = ((end+1)^end) & end;
          
          if (begin >= end-mask) break;
          
          end &= ~mask;
          
          te.event = end;
          te.event_bits = eca_decode_mask(mask);
          result.push_back(te);
          
          --end;
        }
        
        te.event = begin;
        te.event_bits = eca_decode_mask(end-begin);
        result.push_back(te);
      }
    }
  }
  
  sort(result.begin(), result.end(), sort_event_bits_offset_channel);
}

void Table::Impl::compile(std::vector<SearchEntry>& s, std::vector<WalkEntry>& w) const {
  std::vector<TableEntry> t;
  get(t);
  
  s.clear();
  w.clear();
  
  /* !!! For now use the simple containment algorithm */
  
  std::vector<Event> ends;
  std::vector<Index> next;
  
  Event index = 0;
  unsigned ti = 0;
  while (1) {
    /* Push records? */
    while (ti < t.size() && t[ti].event == index) {
      TableEntry& te = t[ti];
      Event mask = eca_encode_mask(te.event_bits);
      Event end = te.event | mask;
      
#ifdef DEBUG
      printf("FROM_USER: %"PRIx64"-%"PRIx64": %"PRIx64" %d %"PRIx32"\n", 
             te.event, end, te.offset, te.channel, te.tag);
#endif
      
      WalkEntry we;
      we.offset  = te.offset;
      we.tag     = te.tag;
      we.channel = te.channel;
      we.next    = next.empty()?-1:next.back();
      
      ends.push_back(end);
      next.push_back(w.size());
      
      w.push_back(we);
#ifdef DEBUG
      printf("WE %d -> %d: %"PRIx64" %d %"PRIx32"\n", ti, we.next, we.offset, we.channel, we.tag);
#endif      
      ++ti;
    }
    
    SearchEntry se;
    se.event = index;
    se.first = next.empty()?-1:next.back();
    s.push_back(se);
#ifdef DEBUG
    printf("SE %d: %"PRIx64" -> %d\n", s.size()-1, index, se.first);
#endif
    
    /* Pop records? */
    if (!ends.empty() && (ti == t.size() || ends.back() < t[ti].event)) {
      index = ends.back();
      while (!ends.empty() && ends.back() == index) {
        ends.pop_back();
        next.pop_back();
      }
      ++index;
      if (index == 0) break;
    } else if (ti != t.size()) {
      index = t[ti].event;
    } else {
      break;
    }
  }
}

}
