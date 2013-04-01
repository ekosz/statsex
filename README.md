# StatsEx - A statsd replacement for Elixir

StatsEx is a drop in replacement for [StatsD](https://github.com/etsy/statsd/).
It responds to subset of the StatsD API, and periodically flushes the data it
collects to Graphite.

## Configuration

StatsEx uses these application variables

* udp\_port: The port that StatsEx listens to - defaults to 8888
* graphite\_host: The host address of the graphite server - defaults to localhost
* graphite\_port: The port of the graphite server - defaults to 2003
