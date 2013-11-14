/** @file eca-table.cpp
 *  @brief Command-line interface to the ECA condition table.
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Supports incremental table configuration.
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

#include <unistd.h> /* getopt */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "eca.h"
#include "lib/version.h"

using namespace GSI_ECA;

static const char* program;
static bool quiet;
static bool verbose;
static bool numeric;

static void help(void) {
  fprintf(stderr, "Usage: %s [OPTION] <etherbone-device> [command]\n", program);
  fprintf(stderr, "\n");
  fprintf(stderr, "  -a <address>  select an ECA unit by Wishbone address\n");
  fprintf(stderr, "  -e <eca-id>   select an ECA unit by index #\n");
  fprintf(stderr, "  -v            verbose operation; report statistics\n");
  fprintf(stderr, "  -q            quiet: do not display table headers\n");
  fprintf(stderr, "  -n            numeric date\n");
  fprintf(stderr, "  -h            display this help and exit\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "  add <event>/<bits> <delay> <channel#> <tag>  modify  program table\n");
  fprintf(stderr, "  del <event>/<bits> <delay> <channel#>        modify  program table\n");
  fprintf(stderr, "  flush                                        clear   program table\n");
  fprintf(stderr, "  dump                                         inspect program table\n");
  fprintf(stderr, "  dump-active                                  inspect active  table\n");
  fprintf(stderr, "  flip-active                                  atomically swap tables\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "Report ECA hardware+software bugs to <w.terpstra@gsi.de>\n");
  fprintf(stderr, "Version %"PRIx32" (%s). Licensed under the LGPL v3.\n",
                  ECA_VERSION_SHORT, ECA_DATE_FULL);
}

static void die(eb_status_t status, const char* what) {
  fprintf(stderr, "%s: %s -- %s\n", program, what, eb_status(status));
  exit(1);
}

static void dump(const ECA& eca, const Table& table) {
  std::vector<TableEntry> t;
  table.get(t);

  
  if (!quiet) {
    if (numeric) {
      printf("--------------------------------------------------------------\n");
      printf("Event ID prefix        Offset              Channel  Tag\n");
      printf("--------------------------------------------------------------\n");
    } else {
      printf("-----------------------------------------------------\n");
      printf("Event ID prefix        Offset     Channel  Tag\n");
      printf("-----------------------------------------------------\n");
    }
  }
  
  for (unsigned i = 0; i < t.size(); ++i) {
    TableEntry& te = t[i];
    
    if (numeric) {
      printf("0x%016"PRIx64"/%-2d  time:0x%016"PRIx64"  %2d  0x%08"PRIx32"\n",
             te.event, te.event_bits, te.offset, te.channel, te.tag);
    } else {
      printf("0x%016"PRIx64"/%-2d  %+8es  %2d  0x%08"PRIx32"\n",
             te.event, te.event_bits, eca.delay(te.offset), te.channel, te.tag);
    }
  }

#if 0
  if (verbose) {
    if (numeric) {
      printf("--------------------------------------------------------------\n");
    } else {
      printf("----------------------------------------------------\n");
    }
    
    std::vector<SearchEntry> st;
    std::vector<WalkEntry> wt;
    rt.compile(st, wt);
    
    printf("Table usage: %d/%d search and %d/%d walk\n",
           (int)st.size(), (int)eca.table_size*2, (int)wt.size(), (int)eca.table_size);
  }
#endif
}

int main(int argc, char** argv) {
  int opt, error;
  char *value_end;
  const char *devpath, *command;
  int eca_id = -1;
  eb_address_t eca_addr = 0;
  bool eca_addr_set = false;
  eb_status_t status;
  
  program = argv[0];
  error = 0;
  
  while ((opt = getopt(argc, argv, "a:e:vqnh")) != -1) {
    switch (opt) {
    case 'a':
      eca_addr = strtoull(optarg, &value_end, 0);
      if (*value_end != 0) {
        fprintf(stderr, "%s: invalid ECA address -- '%s'\n", program, optarg);
        error = 1;
      } else {
        eca_addr_set = true;
      }
      break;
    case 'e':
      eca_id = strtol(optarg, &value_end, 0);
      if (*value_end || eca_id < 0 || eca_id > 100) {
        fprintf(stderr, "%s: invalid ECA id -- '%s'\n", program, optarg);
        error = 1;
      }
      break;
    case 'v':
      verbose = true;
      break;
    case 'q':
      quiet = true;
      break;
    case 'n':
      numeric = true;
      break;
    case 'h':
      help();
      return 0;
    case ':':
    case '?':
      error = 1;
      break;
    default:
      fprintf(stderr, "%s: bad getopt result\n", program);
      return 1;
    }
  }
  
  if (error) return 1;
  
  if (optind >= argc) {
    fprintf(stderr, "%s: expecting one non-optional argument: <etherbone-device>\n", program);
    fprintf(stderr, "\n");
    help();
    return 1;
  }
  
  devpath = argv[optind];
  
  if (optind+1 < argc) {
    command = argv[optind+1];
  } else {
    command = "dump";
  }
  
  if (!strcasecmp(command, "flip-active") ||
      !strcasecmp(command, "dump-active") ||
      !strcasecmp(command, "dump") ||
      !strcasecmp(command, "flush")) {
    if (optind+2 < argc) {
      fprintf(stderr, "%s: unexpected extra arguments -- '%s'\n", program, argv[optind+2]);
      return 1;
    }
  } else if (!strcasecmp(command, "add")) {
    if (optind+6 > argc) {
      fprintf(stderr, "%s: expecting exactly four arguments: add <event>/<bits> <delay> <channel#> <tag>\n", program);
      return 1;
    }
    if (optind+6 < argc) {
      fprintf(stderr, "%s: unexpected extra arguments -- '%s'\n", program, argv[optind+6]);
      return 1;
    }
  } else if (!strcasecmp(command, "del")) {
    if (optind+5 > argc) {
      fprintf(stderr, "%s: expecting exactly three arguments: del <event>/<bits> <delay> <channel#>\n", program);
      return 1;
    }
    if (optind+5 < argc) {
      fprintf(stderr, "%s: unexpected extra arguments -- '%s'\n", program, argv[optind+5]);
      return 1;
    }
  } else {
    fprintf(stderr, "%s: unknown command -- '%s'\n", program, command);
    return 1;
  }
  
  Socket socket;
  if ((status = socket.open()) != EB_OK) die(status, "etherbone::socket.open");
  
  Device device;
  if ((status = device.open(socket, devpath)) != EB_OK) {
    fprintf(stderr, "%s: etherbone::device.open('%s') -- %s\n", program, devpath, eb_status(status));
    return 1;
  }
  
  std::vector<ECA> ecas;
  if ((status = ECA::probe(device, ecas)) != EB_OK) die(status, "ECA::load");
  
  if (ecas.empty()) {
    fprintf(stderr, "%s: no ECA units found\n", program);
    return 1;
  }
  
  if (eca_addr_set) {
    unsigned int i;
    for (i = 0; i < ecas.size(); ++i) {
      if (ecas[i].address == eca_addr) break;
    }
    if (i == ecas.size()) {
      fprintf(stderr, "%s: no ECA found at addres 0x%"EB_ADDR_FMT"\n", program, eca_addr);
      return 1;
    }
    eca_id = (int)i;
  }
  
  /* Select default ECA unit */
  if (eca_id == -1 && ecas.size() == 1) {
    eca_id = 0;
  }
  
  if (eca_id >= (int)ecas.size()) {
    fprintf(stderr, "%s: ECA id '%d' is out of range; max=%d\n", program, eca_id, (int)ecas.size()-1);
    return 1;
  }
  
  ECA& eca = ecas[eca_id];

  TableEntry te;
  int channel;
  
  if (!strcasecmp(command, "add") ||
      !strcasecmp(command, "del")) {
    te.event = strtoull(argv[optind+2], &value_end, 0);
    if (*value_end == '/') {
      te.event_bits = strtol(value_end+1, &value_end, 0);
    } else {
      te.event_bits = 64; /* default to full ID */
    }
    if (*value_end != 0 || te.event_bits < 0 || te.event_bits > 64) {
      fprintf(stderr, "%s: invalid <event>/<bits> argument -- '%s'\n", program, argv[optind+2]);
      return 1;
    }
    
    switch (argv[optind+3][0]) {
    case '+':
    case '-':
      te.offset = eca.delay(strtod(argv[optind+3], &value_end));
      break;
    default:
      te.offset = strtoull(argv[optind+3], &value_end, 0);
      break;
    }
    if (*value_end != 0) {
      fprintf(stderr, "%s: invalid timestamp argument -- '%s'\n", program, argv[optind+3]);
      return 1;
    }
    
    channel = strtol(argv[optind+4], &value_end, 0);
    if (*value_end != 0 || channel < 0 || channel >= (int)eca.channels.size()) {
      fprintf(stderr, "%s: invalid channel# argument -- '%s'\n", program, argv[optind+4]);
      return 1;
    }
    te.channel = channel;
  }
  
  Table table;
  
  if (!strcasecmp(command, "dump")) {
    if (verbose) {
      printf("Retrieving inactive table from ECA #%d \"%s\" (0x%"EB_ADDR_FMT"):\n",
             eca_id, eca.name.c_str(), eca.address);
    }
    
    if ((status = eca.load(false, table)) != EB_OK)
      die(status, "ECA::load(inactive)");
    
    dump(eca, table);
  } 
  
  if (!strcasecmp(command, "dump-active")) {
    if (verbose) {
      printf("Retrieving active table from ECA #%d \"%s\" (0x%"EB_ADDR_FMT"):\n",
             eca_id, eca.name.c_str(), eca.address);
    }
    
    if ((status = eca.load(true, table)) != EB_OK)
      die(status, "ECA::load(active)");
    
    dump(eca, table);
  }
  
  if (!strcasecmp(command, "flip-active")) {
    if (verbose) {
      printf("Retrieving active table from ECA #%d \"%s\" (0x%"EB_ADDR_FMT"):\n",
             eca_id, eca.name.c_str(), eca.address);
    }
    
    if ((status = eca.flipTables()) != EB_OK)
      die(status, "ECA::flipTables");
  }
  
  if (!strcasecmp(command, "flush")) {
    if (verbose) {
      printf("Flushing inactive table on ECA #%d \"%s\" (0x%"EB_ADDR_FMT"):\n",
             eca_id, eca.name.c_str(), eca.address);
    }
    
    /* table is empty */
    if ((status = eca.store(table)) != EB_OK)
      die(status, "ECA::store");
  } 
  
  if (!strcasecmp(command, "add")) {
    te.tag = strtoul(argv[optind+5], &value_end, 0);
    if (*value_end != 0) {
      fprintf(stderr, "%s: invalid tag argument -- '%s'\n", program, argv[optind+5]);
      return 1;
    }
    
    if (verbose) {
      printf("Retrieving inactive table from ECA #%d \"%s\" (0x%"EB_ADDR_FMT"):\n",
             eca_id, eca.name.c_str(), eca.address);
    }
    
    if ((status = eca.load(false, table)) != EB_OK)
      die(status, "ECA::load(inactive)");
    
    if (table.add(te) > 0) {
      fprintf(stderr, "%s: new rule has a tag conflict with existing overlapping rules\n", program);
      return 1;
    }
    
    if (verbose) {
//      printf("Table usage now %d/%d search and %d/%d walk\n",
//             searchs, (int)eca.table_size*2, walks, (int)eca.table_size);
      printf("Programming inactive table on ECA #%d \"%s\" (0x%"EB_ADDR_FMT"):\n",
             eca_id, eca.name.c_str(), eca.address);
    }
    
    if ((status = eca.store(table)) != EB_OK)
      die(status, "ECA::program");
  }
  
  if (!strcasecmp(command, "del")) {
    if (verbose) {
      printf("Retrieving inactive table from ECA #%d \"%s\" (0x%"EB_ADDR_FMT"):\n",
             eca_id, eca.name.c_str(), eca.address);
    }
    
    if ((status = eca.load(false, table)) != EB_OK)
      die(status, "ECA::load(inactive)");
    
    if ((error = table.del(te)) == 0) {
      fprintf(stderr, "%s: no rules were removed\n", program);
      return 1;
    }
    
    if (verbose) {
//      printf("Table usage now %d/%d search and %d/%d walk\n",
//             (int)st.size(), (int)eca.table_size*2, (int)wt.size(), (int)eca.table_size);
      printf("Programming inactive table on ECA #%d \"%s\" (0x%"EB_ADDR_FMT"):\n",
             eca_id, eca.name.c_str(), eca.address);
    }
    
    if ((status = eca.store(table)) != EB_OK)
      die(status, "ECA::program");
  }
  
  device.close();
  socket.close();
  
  return 0;
}

/* !!! warn if too long */
