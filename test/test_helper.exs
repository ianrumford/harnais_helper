ExUnit.start()

defmodule HarnaisHelperHelperTest do
  defmacro __using__(_opts \\ []) do
    quote do
      import HarnaisHelperHelperTest
    end
  end
end
