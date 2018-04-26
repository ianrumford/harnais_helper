defmodule HarnaisHelperTest do
  use ExUnit.Case
  doctest HarnaisHelper

  test "greets the world" do
    assert HarnaisHelper.hello() == :world
  end
end
