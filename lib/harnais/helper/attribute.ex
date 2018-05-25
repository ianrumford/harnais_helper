defmodule Harnais.Helper.Attribute do
  @moduledoc false

  defmacro __using__(_opts \\ []) do
    quote do
      @harnais_helper_key_transform :transform
      @harnais_helper_key_binding :binding
      @harnais_helper_key_expect_error :expect_error
      @harnais_helper_key_expect_value :expect_value
      @harnais_helper_key_expect_text :expect_text
      @harnais_helper_key_expect_texts :expect_texts
      @harnais_helper_key_expect_form :expect_form
      @harnais_helper_key_dictionary :dictionary
      @harnais_helper_key_forms_collection :forms_collection
    end
  end
end
