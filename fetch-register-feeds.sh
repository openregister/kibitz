#!/bin/sh

set -e

cd data/

wget -O country.rsf   https://country.register.gov.uk/download-rsf
wget -O territory.rsf https://territory.register.gov.uk/download-rsf
wget -O uk.rsf        https://uk.discovery.openregister.org/download-rsf

