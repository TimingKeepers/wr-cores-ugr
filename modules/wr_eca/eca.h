/** @file eca.h
 *  @brief C++ Interface to the ECA hardware.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Public API to control all aspects of the Event-Condition-Action Unit.
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

#ifndef ECA_H
#define ECA_H

#include <etherbone.h>
#include <string>
#include <vector>
#include <map>

namespace GSI_ECA {

using namespace etherbone;

typedef uint64_t Time;
typedef uint64_t Event;
typedef uint32_t Tag;
typedef uint32_t Param;
typedef uint8_t  Channel;
typedef int16_t  Index;

/* An action queued to be executed has these fields */
struct ActionEntry {
  Event event;
  Time  time;
  Tag   tag;
  Param param;
};

/* Software condition table entry fields */
struct TableEntry {
  Event   event;
  Time    offset;
  Tag     tag;
  Channel channel;
  uint8_t event_bits;
};

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
struct ReverseTable {
  public:
    /* Returns the number of conflicting records overwritten by this new record */
    int add   (const TableEntry& te);
    int add   (Event begin, Event end, Time, Channel, Tag);
    /* Returns the number of records removed/modified */
    int remove(const TableEntry& te); /* ignores tag */
    int remove(Event begin, Event end, Time, Channel);
    
    /* Convert it to user-friendly form */
    std::vector<TableEntry> reverse() const;
    /* Compile it for loading to hardware */
    void compile(std::vector<SearchEntry>& s, std::vector<WalkEntry>& w) const;
    
    /* Bulk load it from user-friendly table; returns count of conflicting records */
    int load(const std::vector<TableEntry>& t);
    /* Bulk load it from hardware tables; returns count of conflicting records */
    int load(const std::vector<SearchEntry>& s, const std::vector<WalkEntry>& w);
  
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

/* ======================================================================= */
/* Software interface to hardware action channels                          */
/* ======================================================================= */
struct ActionChannel {
  /* ------------------------------------------------------------------- */
  /* Constant hardware values                                            */
  /* ------------------------------------------------------------------- */
  eb_address_t address; /* Wishbone base address of channel */
  std::string  name;    /* Channel instance name */
  
  /* ------------------------------------------------------------------- */
  /* Mutable hardware registers; only modify using methods below         */
  /* ------------------------------------------------------------------- */
  bool         draining; /* Queue is being erased; nothing enters/exits */
  bool         frozen;   /* Queue is frozen;       nothing enters/exits */
  uint16_t     fill;     /* Current number of entries in the queue */
  uint16_t     max_fill; /* Maximum entries in the queue since reset */
  
  /* ------------------------------------------------------------------- */
  /* Access/modify the underlying hardware                               */
  /* ------------------------------------------------------------------- */
  
  /* Reload drain, freeze, fill, max_fill from hardware. */
  status_t refresh(Device dev); 
  
  /* Toggle queue states */
  status_t freeze(Device dev, bool freeze);
  status_t drain (Device dev, bool drain);
  
  /* Clear the queue max_fill counter back to fill */
  status_t reset (Device dev);
};

/* ======================================================================= */
/* Software interface to hardware event streams                            */
/* ======================================================================= */
struct EventStream {
  /* ------------------------------------------------------------------- */
  /* Constant hardware values                                            */
  /* ------------------------------------------------------------------- */
  eb_address_t address;
  
  uint8_t     sdb_ver_major;
  uint8_t     sdb_ver_minor;
  uint32_t    sdb_version;
  uint32_t    sdb_date;
  std::string sdb_name;
  
  /* ------------------------------------------------------------------- */
  /* Access/modify the underlying hardware                               */
  /* ------------------------------------------------------------------- */
  
  /* Send an event to the stream */
  status_t send(Device dev, Event event, Time time, Param param);
};

/* ======================================================================= */
/* Software interface to the ECA hardware                                  */
/* ======================================================================= */
struct ECA {
  /* ------------------------------------------------------------------- */
  /* Constant hardware values                                            */
  /* ------------------------------------------------------------------- */
  
  eb_address_t address;      /* Wishbone base address */
  std::string  name;         /* ECA instance name */
  
  uint8_t     sdb_ver_major; /* API version; major.minor */
  uint8_t     sdb_ver_minor;
  uint32_t    sdb_version;   /* Implementation version */
  uint32_t    sdb_date;      /* Implementation change-date */
  std::string sdb_name;      /* Device category name */
  
  bool inspect_table;        /* Hardware can inspect condition table */
  bool inspect_queue;        /* Hardware can inspect queue contents  */
  
  unsigned table_size;       /* Walker table size, 1/2 search size */
  unsigned queue_size;       /* Maximum actions which can be queued */
  
  uint32_t freq_mul;         /* ECA operating frequency =   */
  uint8_t  freq_5s;          /*  freq_mul*5^freq_5s*2^freq_2s/freq_div */
  uint8_t  freq_2s;
  uint16_t freq_div;
  
  std::vector<ActionChannel> channels; /* Slave channel hardware */
  std::vector<EventStream>   streams;  /* Slave stream hardware  */
  
  /* ------------------------------------------------------------------- */
  /* Mutable hardware registers; only modify using methods below         */
  /* ------------------------------------------------------------------- */
  
  Time time;  /* Time as of the last refresh */
  bool disabled;  /* When disabled, incoming events are dropped */
  
  /* ------------------------------------------------------------------- */
  /* Translate hardware parameters into software-friendly form           */
  /* ------------------------------------------------------------------- */
  
  /* Pretty print the ECA operating frequency */
  std::string frequency(void) const;
  
  /* Format the time/frequency into a date. */
  std::string date(Time time = (Time)-1) const;
  
  /* Transform a delay (in seconds) into an ECA time offset */
  uint64_t delay(double seconds) const;
  double   delay(uint64_t seconds) const;
  
  /* Calculate time/frequency exactly:
   *  seconds    = floor(time/frequency)
   *  subseconds = time/frequency - seconds  ... range=[0, 1)
   */
  void seconds(uint64_t* seconds, double* subseconds, Time t = (Time)-1) const;
  
  /* ------------------------------------------------------------------- */
  /* Access/modify the underlying hardware                               */
  /* ------------------------------------------------------------------- */
  
  status_t refresh   (Device dev); /* refresh time+disabled */
  
  status_t disable   (Device dev, bool disabled); /* Enable/disable the ECA unit */
  status_t flipTables(Device dev);                /* Atomicly flip inactive and active tables  */
  
  /* Load the H/W representation of the tables */
  status_t loadQueue    (Device dev, unsigned channel, std::vector<ActionEntry>& queue);
  status_t loadSearch   (Device dev, bool     active,  std::vector<SearchEntry>& table);
  status_t loadWalk     (Device dev, bool     active,  std::vector<WalkEntry>&   table);
  
  /* Program the INACTIVE table */
  status_t programSearch(Device dev, const std::vector<SearchEntry>& table);
  status_t programWalk  (Device dev, const std::vector<WalkEntry>&   table);
  
  /* Locate all the ECA units on the bus */
  static status_t load(Device dev, std::vector<ECA>& ecas);
};

}

#endif
