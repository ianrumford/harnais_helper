defmodule Harnais.Helper.Mixfile do
  use Mix.Project

  @version "0.1.0"

  def project do
    [
      app: :harnais_helper,
      version: @version,
      description: description(),
      package: package(),
      source_url: "https://github.com/ianrumford/harnais_helper",
      homepage_url: "https://github.com/ianrumford/harnais_helper",
      docs: [extras: ["./README.md", "./CHANGELOG.md"]],
      elixirc_paths: elixirc_paths(Mix.env()),
      elixir: "~> 1.6",
      build_embedded: Mix.env() == :prod,
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp deps do
    [
      {:harnais, "~> 1.0.0"},
      {:ex_doc, "~> 0.18.3", only: :dev}
    ]
  end

  defp elixirc_paths(:test), do: ["lib"]
  defp elixirc_paths(_), do: ["lib"]

  defp package do
    [
      maintainers: ["Ian Rumford"],
      files: ["lib", "mix.exs", "README*", "LICENSE*", "CHANGELOG*"],
      licenses: ["MIT"],
      links: %{github: "https://github.com/ianrumford/harnais_helper"}
    ]
  end

  defp description do
    """
    harnais_helper: Harnais's collection of stand-alone test helpers.
    """
  end
end
