/** @file eca-ctl.cpp
 *  @brief Command-line interface for ECA control registers
 *  @author Wesley W. Terpstra <w.terpstra@gsi.de>
 *
 *  Copyright (C) 2013 GSI Helmholtz Centre for Heavy Ion Research GmbH 
 *
 *  Control registers and queue inspection.
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
  fprintf(stderr, "  -a <address>     select an ECA unit by Wishbone address\n");
  fprintf(stderr, "  -e <eca-id>      select an ECA unit by index #\n");
  fprintf(stderr, "  -c <channel-id>  select an action channel by index #\n");
  fprintf(stderr, "  -s <stream-id>   select an event stream by index #\n");
  fprintf(stderr, "  -v               verbose operation: print SDB records\n");
  fprintf(stderr, "  -q               quiet: do not display table headers\n");
  fprintf(stderr, "  -n               numeric dates\n");
  fprintf(stderr, "  -h               display this help and exit\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "  enable           allow the ECA to receive events\n");
  fprintf(stderr, "  disable          drop all events delivered to the ECA\n");
  fprintf(stderr, "  ienable          allow the ECA to send interrupts\n");
  fprintf(stderr, "  idisable         drop all interrupts from the ECA\n");
  fprintf(stderr, "  status           report all top-level ECA information\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "  activate         allow the channel to emit+receive actions\n");
  fprintf(stderr, "  freeze           prevent hardware from changing Q contents\n");
  fprintf(stderr, "  drain            empty and deactivate the channel\n");
  fprintf(stderr, "  unhook           disable interrupts from this channel\n");
  fprintf(stderr, "  hook <addr>      enable interrupts to <addr> from this channel\n");
  fprintf(stderr, "  reset            reset the channel's max_full counter\n");
  fprintf(stderr, "  inspect          display the contents of a frozen channel\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "  send <event> <param> <tef> <time>  write to an event stream\n");
  fprintf(stderr, "\n");
  fprintf(stderr, "Report ECA hardware+software bugs to <w.terpstra@gsi.de>\n");
  fprintf(stderr, "Version %"PRIx32" (%s). Licensed under the LGPL v3.\n",
                  ECA_VERSION_SHORT, ECA_DATE_FULL);
}

static void die(eb_status_t status, const char* what) {
  fprintf(stderr, "%s: %s -- %s\n", program, what, eb_status(status));
  exit(1);
}

static void render_eca(unsigned i, const ECA& eca) {
  printf("ECA #%d %s  \"%s\" (0x%"EB_ADDR_FMT") -- %s\n", 
    i, eca.disabled?"disabled":"enabled ", eca.name.c_str(), eca.address,
    eca.interrupts?"generating interrupts":"dropping interrupts");
  if (verbose) {
    printf("    API v%d.%d; \"%s\" v%d (%4x-%02x-%02x)\n",
           eca.sdb_ver_major, eca.sdb_ver_minor, eca.sdb_name.c_str(), eca.sdb_version,
           eca.sdb_date >> 16, (eca.sdb_date >> 8) & 0xFF, eca.sdb_date & 0xFF);
  }
  printf("    table:%d,%s  queue:%d,%s  freq:%s\n", eca.table_size, eca.inspect_table?"rw":"wo",
                                                    eca.queue_size, eca.inspect_queue?"rw":"wo",
                                                    eca.frequency().c_str());
  if (numeric)
    printf("    time:0x%"PRIx64"\n", eca.time);
  else
    printf("    %s\n", eca.date().c_str());
  
  for (unsigned c = 0; c < eca.channels.size(); ++c) {
    const ActionChannel& ac = eca.channels[c];
    printf("  Channel #%d %s \"%s\" -- ",
           c, ac.draining?"draining":ac.frozen?"frozen  ":"active  ", ac.name.c_str());
    if (ac.int_enable) {
      printf("sending interrupts to 0x%"PRIx32"\n", ac.int_dest);
    } else {
      printf("dropping interrupts\n");
    }
    printf("    fill:%d, maxfill:%d, total:%d, conflicts:%d, late:%d%s\n",
           ac.fill, ac.max_fill, ac.valid, ac.conflict, ac.late,
           (ac.late||ac.conflict||ac.max_fill==eca.queue_size-1)?" !!!!!!!!":"");
  }
  for (unsigned s = 0; s < eca.streams.size(); ++s) {
    const EventStream& es = eca.streams[s];
    printf("  Stream  #%d (0x%"EB_ADDR_FMT")", s, es.address);
    if (verbose) {
      printf(" - API v%d.%d; \"%s\" v%d (%4x-%02x-%02x)\n",
             es.sdb_ver_major, es.sdb_ver_minor, es.sdb_name.c_str(), es.sdb_version,
             es.sdb_date >> 16, (es.sdb_date >> 8) & 0xFF, es.sdb_date & 0xFF);
    } else {
      printf("\n");
    }
  }
}

static void dump_queue(ECA& eca, ActionChannel& channel) {
  eb_status_t status;
  std::vector<ActionEntry> queue;
  
  if (!quiet) {
    printf("ECA #%d \"%s\" (0x%"EB_ADDR_FMT"), Channel #%d \"%s\"\n",
           eca.index, eca.name.c_str(), eca.address, channel.index, channel.name.c_str());
    if (numeric) {
      printf("----------------------------------------------------------------------------------\n");
      printf("EventID             Param               Tag         Tef         Execution Time\n");
      printf("----------------------------------------------------------------------------------\n");
    } else {
      printf("---------------------------------------------------------------------------------------------\n");
      printf("EventID             Param               Tag         Tef         Execution Time (TAI)\n");
      printf("---------------------------------------------------------------------------------------------\n");
    }
  }
  
  if ((status = channel.load(queue)) != EB_OK)
    die(status, "ActionChannel::load");
  
  for (unsigned i = 0; i < queue.size(); ++i) {
    ActionEntry& ae = queue[i];
    char late = ' ';
    if ((ae.time >> 63) != 0) {
      ae.time = -ae.time;
      late='!';
    }
    
    if (numeric) {
      printf("0x%016"PRIx64"  0x%016"PRIx64"  0x%08"PRIx32"  0x%08"PRIx32" %c0x%016"PRIx64"\n", 
            ae.event, ae.param, ae.tag, ae.tef, late, ae.time);
    } else {
      printf("0x%016"PRIx64"  0x%016"PRIx64"  0x%08"PRIx32"  0x%08"PRIx32" %c%s\n", 
            ae.event, ae.param, ae.tag, ae.tef, late, eca.date(ae.time).c_str());
    }
  }
}

int main(int argc, char** argv) {
  int opt, error;
  char *value_end;
  const char *devpath, *command;
  int eca_id = -1, channel_id = -1, stream_id = -1;
  eb_address_t eca_addr = 0;
  bool eca_addr_set = false;
  eb_status_t status;
  Event event = 0;
  Time time = 0;
  Tef tef = 0;
  Param param = 0;
  eb_address_t hook = 0;
  
  program = argv[0];
  error = 0;
  
  while ((opt = getopt(argc, argv, "a:e:c:s:vqnh")) != -1) {
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
    case 'c':
      channel_id = strtol(optarg, &value_end, 0);
      if (*value_end || channel_id < 0 || channel_id > 30) {
        fprintf(stderr, "%s: invalid channel id -- '%s'\n", program, optarg);
        error = 1;
      }
      break;
    case 's':
      stream_id = strtol(optarg, &value_end, 0);
      if (*value_end || stream_id < 0 || stream_id > 100) {
        fprintf(stderr, "%s: invalid stream id -- '%s'\n", program, optarg);
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
    command = "status";
  }
  
  if (strcasecmp(command, "send") == 0) {
    if (optind+6 > argc) {
      fprintf(stderr, "%s: expecting exactly four arguments: send <event> <param> <tef> <time>\n", program);
      return 1;
    }
    if (optind+6 < argc) {
      fprintf(stderr, "%s: unexpected extra arguments -- '%s'\n", program, argv[optind+6]);
      return 1;
    }
  } else if (strcasecmp(command, "hook") == 0) {
    if (optind+3 > argc) {
      fprintf(stderr, "%s: expecting exactly one argument: hook <address>\n", program);
      return 1;
    }
    if (optind+3 < argc) {
      fprintf(stderr, "%s: unexpected extra arguments -- '%s'\n", program, argv[optind+3]);
      return 1;
    }
  } else {
    if (optind+2 < argc) {
      fprintf(stderr, "%s: unexpected extra arguments -- '%s'\n", program, argv[optind+2]);
      return 1;
    }
  }
  
  if (eca_addr_set && eca_id != -1) {
    fprintf(stderr, "%s: cannot set both -a '0x%"EB_ADDR_FMT"' and -e '%d' at once.\n", 
            program, eca_addr, eca_id);
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
  if ((status = ECA::probe(device, ecas)) != EB_OK) die(status, "ECA::probe");
  
  if (ecas.empty()) {
    fprintf(stderr, "%s: no ECA units found\n", program);
    return 1;
  }
  
  if (eca_addr_set) {
    unsigned i;
    for (i = 0; i < ecas.size(); ++i) {
      if (ecas[i].address == eca_addr) break;
    }
    if (i == ecas.size()) {
      fprintf(stderr, "%s: no ECA found at address 0x%"EB_ADDR_FMT"\n", program, eca_addr);
      return 1;
    }
    eca_id = (int)i;
  }
  
  /* Select default ECA unit */
  if (eca_id == -1 && ecas.size() == 1) {
    eca_id = 0;
  }
  
  if (channel_id != -1 && eca_id == -1) {
    fprintf(stderr, "%s: cannot specify channel id -c '%d' without specifying an ECA unit with -a/-e\n", 
            program, channel_id);
    return 1;
  }
  
  if (stream_id != -1 && eca_id == -1) {
    fprintf(stderr, "%s: cannot specify stream id -s '%d' without specifying an ECA unit with -a/-e\n", 
            program, stream_id);
    return 1;
  }
  
  if (eca_id >= (int)ecas.size()) {
    fprintf(stderr, "%s: ECA id '%d' is out of range; max=%d\n", program, eca_id, (int)ecas.size()-1);
    return 1;
  }
  
  /* Select default channel */
  if (channel_id == -1 && ecas[eca_id].channels.size() == 1) {
    channel_id = 0;
  }
  
  /* Select default stream */
  if (stream_id == -1 && ecas[eca_id].streams.size() == 1) {
    stream_id = 0;
  }
  
  if (channel_id != -1 && channel_id >= (int)ecas[eca_id].channels.size()) {
    fprintf(stderr, "%s: channel id -c '%d' is out of range; max=%d\n", program, channel_id, (int)ecas[eca_id].channels.size()-1);
    return 1;
  }
  
  if (stream_id != -1 && stream_id >= (int)ecas[eca_id].streams.size()) {
    fprintf(stderr, "%s: stream id -s '%d' is out of range; max=%d\n", program, stream_id, (int)ecas[eca_id].streams.size()-1);
    return 1;
  }
  
  /* -------------------------------------------------------------------- */
  if (!strcasecmp(command, "enable")) {
    if (eca_id == -1) {
      fprintf(stderr, "%s: specify an ECA unit to enable with -e/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Enabling ECA #%d \"%s\" (0x%"EB_ADDR_FMT")\n",
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address);
    }
    if ((status = ecas[eca_id].disable(false)) != EB_OK)
      die(status, "ECA::disable(false)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "disable")) {
    if (eca_id == -1) {
      fprintf(stderr, "%s: specify an ECA unit to disable with -e/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Disabling ECA #%d \"%s\" (0x%"EB_ADDR_FMT")\n",
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address);
    }
    if ((status = ecas[eca_id].disable(true)) != EB_OK)
      die(status, "ECA::disable(true)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "status")) {
    if (eca_id == -1) {
      for (unsigned i = 0; i < ecas.size(); ++i) {
        if (i > 0) printf("\n");
        render_eca(i, ecas[i]);
      }
    } else {
      render_eca(eca_id, ecas[eca_id]);
    }
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "ienable")) {
    if (eca_id == -1) {
      fprintf(stderr, "%s: specify an ECA unit to enable interrupts on with -e/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Enabling interrupts on ECA #%d \"%s\" (0x%"EB_ADDR_FMT")\n",
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address);
    }
    if ((status = ecas[eca_id].interrupt(true)) != EB_OK)
      die(status, "ECA:interrupt(true)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "idisable")) {
    if (eca_id == -1) {
      fprintf(stderr, "%s: specify an ECA unit to disable interrupts on with -e/-a\n", program);
      return 1;
    }
    if (verbose) {
      printf("Disabling interrupts on ECA #%d \"%s\" (0x%"EB_ADDR_FMT")\n",
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address);
    }
    if ((status = ecas[eca_id].interrupt(false)) != EB_OK)
      die(status, "ECA:interrupt(false)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "activate")) {
    if (channel_id == -1) {
      fprintf(stderr, "%s: specify a channel to activate with -c\n", program);
      return 1;
    }
    if (verbose) {
      printf("Activating Channel #%d \"%s\" on ECA #%d \"%s\" (0x%"EB_ADDR_FMT")\n",
             channel_id, ecas[eca_id].channels[channel_id].name.c_str(),
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address);
    }
    if ((status = ecas[eca_id].channels[channel_id].drain(false)) != EB_OK)
      die(status, "ActionChannel::drain(false)");
    if ((status = ecas[eca_id].channels[channel_id].freeze(false)) != EB_OK)
      die(status, "ActionChannel::freeze(false)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "freeze")) {
    if (channel_id == -1) {
      fprintf(stderr, "%s: specify a channel to freeze with -c\n", program);
      return 1;
    }
    if (verbose) {
      printf("Freezing Channel #%d \"%s\" on ECA #%d \"%s\" (0x%"EB_ADDR_FMT")\n",
             channel_id, ecas[eca_id].channels[channel_id].name.c_str(),
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address);
    }
    if ((status = ecas[eca_id].channels[channel_id].drain(false)) != EB_OK)
      die(status, "ActionChannel::drain(false)");
    if ((status = ecas[eca_id].channels[channel_id].freeze(true)) != EB_OK)
      die(status, "ActionChannel::freeze(true)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "drain")) {
    if (channel_id == -1) {
      fprintf(stderr, "%s: specify a channel to drain with -c\n", program);
      return 1;
    }
    if (verbose) {
      printf("Draining Channel #%d \"%s\" on ECA #%d \"%s\" (0x%"EB_ADDR_FMT")\n",
             channel_id, ecas[eca_id].channels[channel_id].name.c_str(),
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address);
    }
    if ((status = ecas[eca_id].channels[channel_id].drain(true)) != EB_OK)
      die(status, "ActionChannel::drain(true)");
    if ((status = ecas[eca_id].channels[channel_id].freeze(true)) != EB_OK)
      die(status, "ActionChannel::freeze(true)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "unhook")) {
    if (channel_id == -1) {
      fprintf(stderr, "%s: specify a channel to unhook interrupts with -c\n", program);
      return 1;
    }
    if (verbose) {
      printf("Unhooking Channel #%d \"%s\" on ECA #%d \"%s\" (0x%"EB_ADDR_FMT") from interrupts\n",
             channel_id, ecas[eca_id].channels[channel_id].name.c_str(),
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address);
    }
    if ((status = ecas[eca_id].channels[channel_id].hook(false, 0)) != EB_OK)
      die(status, "ActionChannel::hook(false)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "hook")) {
    if (channel_id == -1) {
      fprintf(stderr, "%s: specify a channel to hook interrupts with -c\n", program);
      return 1;
    }
    
    hook = strtoul(argv[optind+2], &value_end, 0);
    if (*value_end != 0) {
      fprintf(stderr, "%s: invalid address -- '%s'\n", program, argv[optind+2]);
      return 1;
    }

    if (verbose) {
      printf("Hooking Channel #%d \"%s\" on ECA #%d \"%s\" (0x%"EB_ADDR_FMT") to interrupts on 0x%"EB_ADDR_FMT"\n",
             channel_id, ecas[eca_id].channels[channel_id].name.c_str(),
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address, hook);
    }
    if ((status = ecas[eca_id].channels[channel_id].hook(true, hook)) != EB_OK)
      die(status, "ActionChannel::hook(true)");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "reset")) {
    if (channel_id == -1) {
      fprintf(stderr, "%s: specify a channel to reset with -c\n", program);
      return 1;
    }
    if (verbose) {
      printf("Reseting Channel #%d \"%s\" on ECA #%d \"%s\" (0x%"EB_ADDR_FMT")\n",
             channel_id, ecas[eca_id].channels[channel_id].name.c_str(),
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address);
    }
    if ((status = ecas[eca_id].channels[channel_id].reset()) != EB_OK)
      die(status, "ActionChannel::reset()");
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "inspect")) {
    if (channel_id == -1) {
      fprintf(stderr, "%s: specify a channel to inspect with -c\n", program);
      return 1;
    }
    if (!ecas[eca_id].inspect_queue) {
      fprintf(stderr, "%s: ECA #%d was synthesized with read-only queues\n", program, eca_id);
      return 1;
    }
    if (!ecas[eca_id].channels[channel_id].frozen) {
      fprintf(stderr, "%s: channel #%d must be frozen to be inspected\n", program, channel_id);
      return 1;
    }
    dump_queue(ecas[eca_id], ecas[eca_id].channels[channel_id]);
  }
  
  /* -------------------------------------------------------------------- */
  else if (!strcasecmp(command, "send")) {
    if (stream_id == -1) {
      fprintf(stderr, "%s: specify a stream to send with -s\n", program);
      return 1;
    }
    
    event = strtoull(argv[optind+2], &value_end, 0);
    if (*value_end != 0) {
      fprintf(stderr, "%s: invalid event-ID -- '%s'\n", program, argv[optind+2]);
      return 1;
    }
    
    param = strtoull(argv[optind+3], &value_end, 0);
    if (*value_end != 0) {
      fprintf(stderr, "%s: invalid param -- '%s'\n", program, argv[optind+3]);
      return 1;
    }
    
    tef = strtoull(argv[optind+4], &value_end, 0);
    if (*value_end != 0) {
      fprintf(stderr, "%s: invalid tef -- '%s'\n", program, argv[optind+4]);
      return 1;
    }
    
    if (argv[optind+5][0] == '+') {
      double off = strtod(&argv[optind+5][1], &value_end);
      if (*value_end != 0 && (value_end[0] != 's' && value_end[1] != 0)) {
        fprintf(stderr, "%s: invalid time -- '%s'\n", program, argv[optind+5]);
        return 1;
      }
      time = ecas[eca_id].time + ecas[eca_id].delay(off);
    } else {
      time = strtoull(argv[optind+5], &value_end, 0);
      if (*value_end != 0) {
        fprintf(stderr, "%s: invalid time -- '%s'\n", program, argv[optind+5]);
        return 1;
      }
    }
    
    if (verbose) {
      printf("Sending to Stream #%d (0x%"EB_ADDR_FMT") on ECA #%d \"%s\" (0x%"EB_ADDR_FMT"): 0x%"PRIx64" 0x%"PRIx64" 0x%"PRIx32" 0x%"PRIx64"\n",
             stream_id, ecas[eca_id].streams[stream_id].address,
             eca_id, ecas[eca_id].name.c_str(), ecas[eca_id].address,
             event, param, tef, time);
    }
    if ((status = ecas[eca_id].streams[stream_id].send(EventEntry(event, param, tef, time))) != EB_OK)
      die(status, "EventStream::send");
  }
  
  /* -------------------------------------------------------------------- */
  else {
    fprintf(stderr, "%s: unknown command -- '%s'\n", program, command);
    return 1;
  }
  
  device.close();
  socket.close();
  
  return 0;
}
