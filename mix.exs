defmodule Statsex.Mixfile do
  use Mix.Project

  def project do
    [app: :statsex,
     version: "0.0.1",
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps() ]
  end

  def application do
    [extra_applications: [:logger],
     mod: { StatsEx.Application, []}]
  end

  defp deps do
    [
      {:credo, "~> 0.6", only: [:dev, :test]},
    ]
  end
end
