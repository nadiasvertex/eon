#!/usr/bin/env bash
fleetctl start ceph-mon@{1,2,3}
fleetctl start ceph-mds@1
fleetctl start ceph-osd@{1,2,3}

