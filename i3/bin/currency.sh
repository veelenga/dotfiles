#!/bin/sh
# Copyright (C) 2014 Vitaliy Elengaupt <velenhaupt@gmail.com>
 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Usage. To change default parameters:
# currency.sh EUR UAH
#
from=${1:-"USD"}
to=${2:-"UAH"}

# api describtion: https://github.com/hippasus/ExchangeRates
curl -s http://rate-exchange.appspot.com/currency\?from\=$from\&to\=$to |
  python -c "import sys,json; data=json.loads(sys.stdin.read()); print(data['rate'] if 'rate' in data.keys() else 'N/A')"
