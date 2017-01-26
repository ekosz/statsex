use Mix.Config

config :statsex,
  udp_port: 8888,
  graphite_host: '127.0.0.1',
  graphite_port: 2003,
  environment_name: Mix.env

config :logger,
  level: :debug,
  truncate: 4096,
  backends: [:console]

# import_config "#{Mix.env}.exs"