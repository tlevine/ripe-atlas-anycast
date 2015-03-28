#!/usr/bin/env python
import sys, csv, json

columns = [
  "af",
  "asn_v4",
  "asn_v6",
  "country_code",
  "dist",
  "dst_addr",
  "dst_lat",
  "dst_lon",
  "from",
  "fw",
  "group_id",
  "lts",
  "msm_id",
  "msm_name",
  "prb_id",
  "proto",
  "src_addr",
  "src_lat",
  "src_lon",
  "timestamp",
  "type",
]

with open(sys.argv[1]) as fp:
    anycast = json.load(fp)
writer = csv.writer(sys.stdout)

writer.writerow(columns)
for row in anycast:
    writer.writerow([row[column] for column in columns])
