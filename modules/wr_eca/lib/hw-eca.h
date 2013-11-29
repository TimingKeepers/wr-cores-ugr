/** @file hw-eca.h
 *  @brief Register layout of the ECA unit.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  All offsets for regsiters in the ECA unit.
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

#ifndef ECA_HW_H
#define ECA_HW_H

#include <etherbone.h>
#include <map>

#define GSI_VENDOR_ID	0x651
#define ECA_DEVICE_ID	0x8752bf44U
#define ECAE_DEVICE_ID	0x8752bf45U

#define ECA_FEATURE_INSPECT_TABLE 0x1
#define ECA_FEATURE_INSPECT_QUEUE 0x2

#define ECA_CTL_DISABLE		0x01
#define ECA_CTL_INT_ENABLE	0x02
#define ECA_CTL_FLIP		0x04

#define ECA_INFO	0x00
#define ECA_TABLE_SIZE	0x00
#define ECA_QUEUE_SIZE	0x01
#define ECA_NUM_CHANNELS 0x02
#define ECA_INDEX	0x03
#define ECA_CTL		0x04
#define ECA_FEATURE	0x04
#define ECA_NAME	0x05
#define ECA_CTL_CLEAR	0x06
#define ECA_CTL_SET	0x07
#define ECA_TIME1	0x08
#define ECA_TIME0	0x0C
#define ECA_SEARCH	0x10
#define ECA_FIRST	0x14
#define ECA_EVENT1	0x18
#define ECA_EVENT0	0x1C
#define ECA_WALK	0x20
#define ECA_NEXT	0x24
#define ECA_DELAY1	0x28
#define ECA_DELAY0	0x2C
#define ECA_TAG		0x30
#define ECA_CHANNEL	0x34
#define ECA_FREQ_MUL	0x38
#define ECA_FREQ_5S	0x3C
#define ECA_FREQ_2S	0x3D
#define ECA_FREQ_DIV	0x3E

#define ECAQ_STATUS_VALID	0x01
#define ECAQ_STATUS_LATE	0x02

#define ECAQ_CTL_DRAIN		0x01
#define ECAQ_CTL_FREEZE		0x02
#define ECAQ_CTL_INT_MASK	0x04

#define ECAQ_SELECT	0x40
#define ECAQ_CHANNEL	0x40
#define ECAQ_INDEX	0x42
#define ECAQ_CTL	0x44
#define ECAQ_STATUS	0x44
#define ECAQ_NAME	0x45
#define ECAQ_CTL_CLEAR	0x46
#define ECAQ_CTL_SET	0x47
#define ECAQ_INT_DEST	0x48
#define ECAQ_FILL	0x50
#define ECAQ_MAX_FILL	0x52
#define ECAQ_VALID	0x54
#define ECAQ_CONFLICT	0x58
#define ECAQ_LATE	0x5C

#define ECAQ_EVENT1	0x60
#define ECAQ_EVENT0	0x64
#define ECAQ_PARAM1	0x68
#define ECAQ_PARAM0	0x6C
#define ECAQ_TAG	0x70
#define ECAQ_TEF	0x74
#define ECAQ_TIME1	0x78
#define ECAQ_TIME0	0x7C

namespace GSI_ECA {

/* Hardware condition search table fields */
struct SearchEntry {
  Event event;
  Index first; /* -1 if end-of-list */
};

/* Hardware condition walk table fields */
struct WalkEntry {
  Time    offset;
  Tag     tag;
  Index   next; /* -1 if end-of-list */
  Channel channel;
};

/* A useful intermediate format for the condition table */
struct Table::Impl {
  public:
    /* Returns the number of conflicting records overwritten by this new record */
    int add   (const TableEntry& te);
    int add   (Event begin, Event end, Time, Channel, Tag);
    /* Returns the number of records removed/modified */
    int remove(const TableEntry& te); /* ignores tag */
    int remove(Event begin, Event end, Time, Channel);
    
    /* Convert it to user-friendly form */
    void get(std::vector<TableEntry>& te) const;
    /* Bulk load it from user-friendly table; returns count of conflicting records */
    int set(const std::vector<TableEntry>& t);
    
    /* Bulk load it from hardware tables; returns count of conflicting records */
    int decompile(const std::vector<SearchEntry>& s, const std::vector<WalkEntry>& w);
    /* Compile it for loading to hardware */
    void compile(std::vector<SearchEntry>& s, std::vector<WalkEntry>& w) const;
    
  protected:
    struct EventRange {
      Event end; /* [key, end] */
      Tag   tag;
    };
    typedef std::map<Event, EventRange> EventFilter;
    typedef std::map<Time, EventFilter> TableActions;
    typedef std::vector<TableActions>   ChannelMap;
    
    ChannelMap data;
};

}

#endif
