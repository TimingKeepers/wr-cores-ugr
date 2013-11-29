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

namespace GSI_ECA {

using namespace etherbone;

typedef uint8_t  Channel;
typedef uint64_t Event;
typedef int16_t  Index;
typedef uint64_t Param;
typedef uint32_t Tag;
typedef uint32_t Tef;
typedef uint64_t Time;

/* An Event sent to the ECA has these fields */
struct EventEntry {
  Event event;
  Param param;
  Tef   tef;
  Time  time;
  
  EventEntry(Event e = 0, Param p = 0, Tef t = 0, Time i = 0)
   : event(e), param(p), tef(t), time(i) { }
};

/* An action queued to be executed has these fields */
struct ActionEntry {
  Event event;
  Param param;
  Tag   tag;
  Tef   tef;
  Time  time;
  
  ActionEntry(Event e = 0, Param p = 0, Tag a = 0, Tef t = 0, Time i = 0)
   : event(e), param(p), tag(a), tef(t), time(i) { }
};

/* Software condition table entry fields */
struct TableEntry {
  Event   event;
  Time    offset;
  Tag     tag;
  Channel channel;
  uint8_t event_bits;
  
  TableEntry(Event e = 0, Time o = 0, Tag t = 0, Channel c = 0, uint8_t b = 0)
   : event(e), offset(o), tag(t), channel(c), event_bits(b) { }
};

/* Condition table */
class Table {
  private:
    struct Impl;
    Impl *impl;
    
  public:
    Table();
    Table(const Table& table);
    ~Table();
    
    void swap(Table& table);
    Table& operator = (Table x);
    
    int add(const TableEntry& te); /* Returns # records overwritten (conflict) */
    int del(const TableEntry& te); /* Returns # records removed/modified */
    
    /* Bulk import/export of entries */
    int  set(const std::vector<TableEntry>& vect);
    void get(std::vector<TableEntry>& vect) const;
  
  friend struct ECA;
};

/* ======================================================================= */
/* Software interface to hardware action channels                          */
/* ======================================================================= */
struct ActionChannel {
  /* ------------------------------------------------------------------- */
  /* Constant hardware values                                            */
  /* ------------------------------------------------------------------- */
  std::string  name;         /* Channel instance name */
  unsigned     queue_size;   /* Size of the inspectable queue */
  
  /* ------------------------------------------------------------------- */
  /* Mutable hardware registers; only modify using methods below         */
  /* ------------------------------------------------------------------- */
  bool         draining;   /* Queue is being erased; nothing enters/exits */
  bool         frozen;     /* Queue is frozen;       nothing enters/exits */
  bool         int_enable; /* Are interrupts for this channel enabled? */
  uint32_t     int_dest;   /* Destination for interrupts */
  uint16_t     fill;       /* Current number of entries in the queue */
  uint16_t     max_fill;   /* Maximum entries in the queue since reset */
  uint32_t     valid;      /* How many valid actions have been sent (includes late+conflict) */
  uint32_t     conflict;   /* How many conflicting actions have been sent */
  uint32_t     late;       /* How many late  actions have been sent */
  
  /* ------------------------------------------------------------------- */
  /* Access/modify the underlying hardware                               */
  /* ------------------------------------------------------------------- */
  
  Device       device;       /* Device which hosts this ECA */
  eb_address_t address;      /* Wishbone base address */
  Channel      index;        /* Index of the channel */
  
  /* Reload drain, freeze, fill, max_fill from hardware. */
  status_t refresh(); 
  /* Clear all counters (max_fill, valid, late) */
  status_t reset();
  
  /* Toggle queue states */
  status_t freeze(bool freeze);
  status_t drain (bool drain);
  
  /* Hook/unhook interrupt handling */
  status_t hook(bool enable, uint32_t address);
  
  /* Grab the contents from a frozen channel */
  status_t load(std::vector<ActionEntry>& queue);
};

/* ======================================================================= */
/* Software interface to hardware event streams                            */
/* ======================================================================= */
struct EventStream {
  /* ------------------------------------------------------------------- */
  /* Constant hardware values                                            */
  /* ------------------------------------------------------------------- */
  uint8_t     sdb_ver_major;
  uint8_t     sdb_ver_minor;
  uint32_t    sdb_version;
  uint32_t    sdb_date;
  std::string sdb_name;
  
  /* ------------------------------------------------------------------- */
  /* Access/modify the underlying hardware                               */
  /* ------------------------------------------------------------------- */
  
  Device       device;  /* Device which hosts this ECA */
  eb_address_t address; /* Base address of the event stream */
  
  /* Send an event to the stream */
  status_t send(EventEntry e);
};

/* ======================================================================= */
/* Software interface to the ECA hardware                                  */
/* ======================================================================= */
struct ECA {
  /* ------------------------------------------------------------------- */
  /* Constant hardware values                                            */
  /* ------------------------------------------------------------------- */
  
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
  
  Time time;       /* Time as of the last refresh */
  bool disabled;   /* When disabled, incoming events are dropped */
  bool interrupts; /* Gnerate interrupts? See also ActionChannel.int_enable */
  
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
  
  Device       device;       /* Device which hosts this ECA */
  eb_address_t address;      /* Wishbone base address */
  unsigned     index;        /* Index of the ECA */
  
  status_t refresh(); /* refresh time+disabled */
  
  status_t disable  (bool disabled);   /* Enable/disable the ECA unit */
  status_t interrupt(bool interrupts); /* Enable/disable interrupts */
  status_t flipTables();               /* Atomicly flip inactive and active tables  */
  
  /* Load/store the condition table */
  status_t load(bool active, Table& table);
  status_t store(const Table& table);
  
  /* Locate all the ECA units on the bus */
  static status_t probe(Device dev, std::vector<ECA>& ecas);
};


/* ======================================================================= */
/* Inline functions that are not part of the ABI                           */
/* ======================================================================= */

inline void Table::swap(Table& table) {
  Impl* tmp = impl;
  impl = table.impl;
  table.impl = tmp;
}

inline Table& Table::operator = (Table x) {
  swap(x);
  return *this;
}

}

#endif
