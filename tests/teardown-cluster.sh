#!/usr/bin/env bash
fleetctl destroy ceph-mon@{1,2,3}
fleetctl destroy ceph-mds@1
fleetctl destroy ceph-osd@{1,2,3}

