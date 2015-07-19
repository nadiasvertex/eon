#!/bin/sh
pushd ..
docker build -t eon .
popd
vagrant up
vagrant ssh eon-01 -c "cd $HOME/workspace/eon/tests && fleetctl start eon@{1,2,3}"
