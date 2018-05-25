defmodule Harnais.Helper do
  @moduledoc ~S"""
  `Harnais.Helper` is a collection of low-level test helpers, especially for doctests.

  ## Documentation Terms

  In the documentation these terms, usually in *italics*, are used to mean the same thing.

  ### *form* and *forms*

  A *form* is a quoted form (`Macro.t`). A *forms* is a list of zero, one or more *form*s.
  """

  use Harnais.Helper.Attribute

  import Harnais.Utility,
    only: [
      map_collate0_enum: 2,
      opts_create_aliases_dict: 1,
      opts_canonical_keys: 2,
      form_validate: 1,
      forms_validate: 1,
      forms_normalise: 1,
      forms_reduce: 1,
      list_wrap_flat_just: 1
    ]

  @type error :: Harnais.struct()

  @harnais_helper_eval_form_kvs_aliases [
    {@harnais_helper_key_forms_collection, [:forms]},
    {@harnais_helper_key_dictionary, nil},
    {@harnais_helper_key_transform, nil},
    {@harnais_helper_key_binding, nil},
    {@harnais_helper_key_expect_value, [:compare_value, :result, :compare_result]},
    {@harnais_helper_key_expect_text, [:text, :compare_text]},
    {@harnais_helper_key_expect_texts, [:texts, :compare_texts]},
    {@harnais_helper_key_expect_form, [:compare_form, :expect_ast, :compare_ast]},
    {@harnais_helper_key_expect_error, [:compare_error, :error]}
  ]

  @harnais_helper_eval_form_dict_aliases @harnais_helper_eval_form_kvs_aliases
                                         |> opts_create_aliases_dict

  @doc false
  @since "0.1.0"

  def harnais_helper_eval_form_opts_canon_keys!(
        opts,
        dict \\ @harnais_helper_eval_form_dict_aliases
      ) do
    with {:ok, opts} <- opts |> opts_canonical_keys(dict) do
      opts
    else
      {:error, %{__struct__: _} = error} -> raise error
    end
  end

  @doc false
  @since "0.1.0"

  def harnais_helper_eval_form_normalise_error(error, opts \\ [])

  def harnais_helper_eval_form_normalise_error(error, _opts) do
    cond do
      is_binary(error) -> error
      Exception.exception?(error) -> error |> Exception.message()
      is_atom(error) -> error |> to_string
      true -> error |> inspect
    end
  end

  @doc false
  @since "0.1.0"

  def harnais_helper_eval_form_compare(actual, expect_key, opts \\ [])

  def harnais_helper_eval_form_compare(actual, @harnais_helper_key_expect_text = expect_key, opts)
      when is_list(opts) do
    with {:ok, actual_clean} <- actual |> harnais_helper_clean_text do
      # any result to compare?
      case opts |> Keyword.has_key?(expect_key) do
        true ->
          with {:ok, expect_clean} <- opts |> Keyword.get(expect_key) |> harnais_helper_clean_text do
            case expect_clean == actual_clean do
              true ->
                {:ok, {actual_clean, actual}}

              _ ->
                {:error,
                 %ArgumentError{
                   message:
                     "mismatch; expect_key #{inspect(expect_key)}; expect_text: #{
                       inspect(expect_clean)
                     }; actual_text: #{inspect(actual_clean)}"
                 }}
            end
          else
            {:error, %{__exception__: true}} = result -> result
          end

        # nothing to do
        _ ->
          {:ok, {actual_clean, actual}}
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  def harnais_helper_eval_form_compare(ast, @harnais_helper_key_expect_texts = expect_key, opts)
      when is_list(opts) do
    with {:ok, actual_texts} <- ast |> helper_asts_clean_text do
      # any result to compare?
      case opts |> Keyword.has_key?(expect_key) do
        true ->
          with {:ok, expect_texts} <- opts |> Keyword.get(expect_key) |> helper_asts_clean_text do
            case expect_texts == actual_texts do
              true ->
                {:ok, {actual_texts, ast}}

              _ ->
                {:error,
                 %ArgumentError{
                   message:
                     "mismatch; expect_key #{inspect(expect_key)}; expect_texts: #{
                       inspect(expect_texts)
                     }; actual_texts: #{inspect(actual_texts)}"
                 }}
            end
          else
            {:error, %{__exception__: true}} = result -> result
          end

        # nothing to do
        _ ->
          {:ok, {actual_texts, ast}}
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  def harnais_helper_eval_form_compare(ast, @harnais_helper_key_expect_form = expect_key, opts)
      when is_list(opts) do
    # need to reduce the ast(s) first and eval that.
    with {:ok, actual_form} <- ast |> forms_reduce do
      actual_text = actual_form |> Macro.to_string()

      # anything to compare?
      case opts |> Keyword.has_key?(expect_key) do
        true ->
          expect_form = opts |> Keyword.get(expect_key)

          case expect_form == actual_form do
            true ->
              {:ok, {actual_text, actual_form}}

            _ ->
              {:error,
               %ArgumentError{
                 message:
                   "mismatch; expect_key #{inspect(expect_key)}; expect_form: #{
                     inspect(expect_form)
                   }; actual_form: #{inspect(actual_form)}"
               }}
          end

        # nothing to do
        _ ->
          {:ok, {actual_text, actual_form}}
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  def harnais_helper_eval_form_compare(ast, @harnais_helper_key_binding = expect_key, opts)
      when is_atom(expect_key) and is_list(opts) do
    case opts |> Keyword.has_key?(expect_key) do
      x when x in [nil, false] ->
        {:ok, {nil, nil, ast}}

      _ ->
        binding = opts |> Keyword.get(expect_key)

        # need to reduce the ast(s) first and eval that.
        with {:ok, actual_form} <- ast |> forms_reduce,
             {:ok, actual_text} <- actual_form |> harnais_helper_clean_text,
             {actual_value, _binding} <- actual_form |> Code.eval_quoted(binding, __ENV__) do
          # any compares?
          with {:ok, _} <-
                 harnais_helper_eval_form_compare(
                   actual_value,
                   @harnais_helper_key_expect_value,
                   opts
                 ),
               {:ok, _} <-
                 harnais_helper_eval_form_compare(
                   actual_text,
                   @harnais_helper_key_expect_text,
                   opts
                 ),
               {:ok, _} <-
                 harnais_helper_eval_form_compare(
                   actual_form,
                   @harnais_helper_key_expect_form,
                   opts
                 ) do
            # compares ok
            {:ok, {actual_value, [actual_text], actual_form}}
          else
            {:error, %{__exception__: true}} = result -> result
          end
        else
          error -> {:error, %ArgumentError{message: "eval_failed; reason: #{inspect(error)}"}}
        end
    end
  end

  def harnais_helper_eval_form_compare(actual, expect_key, opts)
      when is_atom(expect_key) and is_list(opts) do
    # any result to compare?
    case opts |> Keyword.has_key?(expect_key) do
      true ->
        expect = opts |> Keyword.get(expect_key)

        case expect == actual do
          true ->
            {:ok, actual}

          _ ->
            {:error,
             %ArgumentError{
               message:
                 "mismatch; expect_key #{inspect(expect_key)}; expect_tests: #{inspect(expect)}; actual: #{
                   inspect(actual)
                 }"
             }}
        end

      # nothing to do
      _ ->
        {:ok, actual}
    end
  end

  @doc false
  @since "0.1.0"

  # returns {:ok, {result, texts, ast}} or {:error, error}.
  # the returned ast will be the reduced one used for eval i.e. after any transforms.
  # if no binding the result will be nil.

  def harnais_helper_eval_form(ast, opts \\ [])

  def harnais_helper_eval_form(nil, _opts) do
    {:ok, {nil, [""], nil}}
  end

  def harnais_helper_eval_form(ast, opts) when is_list(opts) do
    opts = opts |> harnais_helper_eval_form_opts_canon_keys!

    with {:ok, ast} <- ast |> harnais_helper_transform(opts),
         {:ok, {actual_texts, ast}} <-
           ast |> harnais_helper_eval_form_compare(@harnais_helper_key_expect_texts, opts),
         {:ok, {actual_value, _actual_text, actual_form}} <-
           ast |> harnais_helper_eval_form_compare(@harnais_helper_key_binding, opts) do
      {:ok, {actual_value, actual_texts, actual_form}}
    else
      # if an error and matches the expected error then return {:ok, {actual_error, nil, nil}}
      {:error, error} ->
        case opts |> Keyword.has_key?(@harnais_helper_key_expect_error) do
          true ->
            error
            |> harnais_helper_eval_form_normalise_error
            |> harnais_helper_eval_form_compare(@harnais_helper_key_expect_error, opts)
            |> case do
              # expected error matches
              {:ok, actual_error} ->
                {:ok, {actual_error, nil, nil}}

              _ ->
                {:error, error}
            end

          # passthru
          _ ->
            {:error, error}
        end
    end
  end

  @doc false
  @since "0.1.0"

  def harnais_helper_transform(ast, opts \\ [])

  def harnais_helper_transform(ast, []) do
    {:ok, ast}
  end

  def harnais_helper_transform(ast, opts) do
    fun_transform =
      cond do
        Keyword.has_key?(opts, @harnais_helper_key_transform) ->
          Keyword.get(opts, @harnais_helper_key_transform)

        # default is to anonymise vars
        true ->
          &harnais_helper_transform_anonymise_vars/1
      end
      |> list_wrap_flat_just
      |> case do
        [transform] ->
          transform

        transforms ->
          fn ast -> transforms |> Enum.reduce(ast, fn f, s -> f.(s) end) end
      end

    {:ok, fun_transform.(ast)}
  end

  @doc false
  @since "0.1.0"

  def harnais_helper_transform_anonymise_vars(ast) do
    ast
    |> Macro.postwalk(fn
      {name, [], mod} when is_atom(name) and is_atom(mod) -> Macro.var(name, nil)
      x -> x
    end)
  end

  @code_edits_beg [
    # dump too many leading or trailing space after \n
    {~r/\A\n\s\s*/s, "\n "},
    {~r/\n\s+\z/s, "\n"}
  ]

  @code_edits_mid [
    {~r/ case /, " case() "}
  ]

  @code_edits_fin [
    {~r/\-\>\n/, "-> "},
    {~r/\n\|\>/, " |>"},

    # dump too many leading or trailing space after \n
    {~r/\A\n\s\s*/s, "\n "},
    {~r/\n\s+\z/s, "\n"},

    # tidy front
    {~r/\A\(\n\s+/s, "("},

    # tidy back
    {~r/\n\s+\)\z/s, ")"}
  ]

  defp harnais_helper_edits(code, edits) when is_binary(code) do
    edits
    |> Enum.reduce(code, fn {r, v}, s ->
      Regex.replace(r, s, v)
    end)
  end

  @doc false
  @since "0.1.0"

  def harnais_helper_clean_text(code)

  def harnais_helper_clean_text(code) when is_binary(code) do
    text =
      code
      |> harnais_helper_edits(@code_edits_beg)
      |> String.split("\n")
      |> Enum.reject(fn str -> String.length(str) == 0 end)
      |> Enum.map(&String.trim/1)
      |> Enum.map(fn str ->
        str |> harnais_helper_edits(@code_edits_mid)
      end)
      |> Enum.reject(fn str -> String.length(str) == 0 end)
      |> Enum.join("\n ")
      |> (fn code ->
            code |> harnais_helper_edits(@code_edits_fin)
          end).()

    {:ok, text}
  end

  def harnais_helper_clean_text(code) do
    with {:ok, ast} <- code |> form_validate do
      ast |> Macro.to_string() |> harnais_helper_clean_text
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc false
  @since "0.1.0"

  def helper_asts_clean_text(asts) do
    asts
    |> List.wrap()
    |> Enum.reject(&is_nil/1)
    |> Enum.reduce_while(
      [],
      fn ast, texts ->
        case ast |> harnais_helper_clean_text do
          {:ok, text} -> {:cont, [text | texts]}
          {:error, %{__struct__: _}} = result -> {:halt, result}
        end
      end
    )
    |> case do
      {:error, %{__exception__: true}} = result -> result
      texts -> {:ok, texts |> Enum.reverse()}
    end
  end

  @doc false
  @since "0.1.0"

  def harnais_helper_compare_form(actual, expect) do
    with {:ok, actual_code} <- actual |> harnais_helper_clean_text,
         {:ok, expect_code} <- expect |> harnais_helper_clean_text do
      case Kernel.==(actual_code, expect_code) do
        true ->
          {:ok, actual}

        _ ->
          {:error,
           %ArgumentError{
             message:
               "expect_actual_mismatch; expect_code #{inspect(expect_code)}; actual_code #{
                 inspect(actual_code)
               }"
           }}
      end
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc false
  @since "0.1.0"

  def harnais_helper_compare_form!(actual_code, expect_code) do
    with {:ok, actual} <- harnais_helper_compare_form(actual_code, expect_code) do
      actual
    else
      {:error, error} -> raise error
    end
  end

  @doc ~S"""
  `harnais_helper_show_forms/1` takes a *forms*, normalises them,
  converts each to a text string (`Macro.to_string/1`), "cleans" each
  text and returns `{:ok, texts}`.

  ## Examples

      iex> [quote(do: x   = x + 1),
      ...>  quote(do: x = x   * x ),
      ...>  quote(do: x=x-1   )
      ...> ] |> harnais_helper_show_forms
      {:ok, ["x = x + 1", "x = x * x", "x = x - 1"]}

      iex> [quote(do: x = x + 1),
      ...>  quote(do: x = x * x),
      ...>  quote(do: x = x - 1)
      ...> ] |> harnais_helper_show_forms!
      ["x = x + 1", "x = x * x", "x = x - 1"]
  """

  @since "0.1.0"

  @spec harnais_helper_show_forms(any, any) :: {:ok, [String.t()]} | {:error, error}

  def harnais_helper_show_forms(forms, _opts \\ []) do
    with {:ok, forms} <- forms |> forms_validate do
      forms
      |> map_collate0_enum(&harnais_helper_clean_text/1)
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @since "0.1.0"

  @spec harnais_helper_show_forms!(any, any) :: [String.t()] | no_return

  def harnais_helper_show_forms!(forms, opts \\ []) do
    with {:ok, forms} <- forms |> harnais_helper_show_forms(opts) do
      forms
    else
      {:error, error} -> raise error
    end
  end

  @doc ~S"""
  `harnais_helper_format_forms/1` takes a *forms*, normalises them,
  converts each to a text string (`Macro.to_string/1`) and then
  applies the standard Elixir code formatter (`Code.format_string!/2`)
  to each text, splits on line breaks ("\n") and returns `{:ok, texts}`.

  ## Examples

      iex> [quote(do: x   = x + 1),
      ...>  quote(do: x = x   * x ),
      ...>  quote(do: x=x-1   )
      ...> ] |> harnais_helper_format_forms
      {:ok, ["x = x + 1", "x = x * x", "x = x - 1"]}

      iex> [quote(do: x = x + 1),
      ...>  quote(do: x = x * x),
      ...>  quote(do: x = x - 1)
      ...> ] |> harnais_helper_format_forms!
      ["x = x + 1", "x = x * x", "x = x - 1"]

      iex> quote do
      ...>   def what_is_x(x) do
      ...>     x
      ...>     |> case do
      ...>  x when is_atom(x) -> :atom
      ...>  x when is_binary(x) ->
      ...> :string
      ...>                     x when is_integer(x) ->
      ...>    # x is an integer
      ...>         :integer
      ...>                 _ -> :no_idea
      ...>     end
      ...>   end
      ...> end
      ...> |> harnais_helper_format_forms!
      ["def(what_is_x(x)) do",
       "  x",
       "  |> case do",
       "    x when is_atom(x) ->",
       "      :atom",
       "",
       "    x when is_binary(x) ->",
       "      :string",
       "",
       "    x when is_integer(x) ->",
       "      :integer",
       "",
       "    _ ->",
       "      :no_idea",
       "  end",
       "end"]

  """

  @since "0.1.0"

  @spec harnais_helper_format_forms(any, any) :: {:ok, [String.t()]} | {:error, error}

  def harnais_helper_format_forms(forms, _opts \\ []) do
    with {:ok, forms} <- forms |> forms_normalise do
      forms =
        forms
        |> Enum.flat_map(fn form ->
          form
          |> Macro.to_string()
          |> Code.format_string!()
          |> Enum.join()
          |> String.split("\n")
        end)

      {:ok, forms}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @since "0.1.0"

  @spec harnais_helper_format_forms!(any, any) :: [String.t()] | no_return

  def harnais_helper_format_forms!(forms, opts \\ []) do
    with {:ok, forms} <- forms |> harnais_helper_format_forms(opts) do
      forms
    else
      {:error, error} -> raise error
    end
  end

  @doc false
  @doc ~S"""
  `harnais_helper_test_forms/1` takes a *forms* and an *opts* with a *binding*.

  The *forms* are normalised and evaluated with the *binding* (`Code.eval_quoted/3`).

  The *forms* are also passed through `harnais_helper_show_forms/1`.

  The result of both are combined to returns `{:ok, {eval_result, forms_texts}}`.

  ## Examples

      iex> [quote(do: x = x + 1),
      ...>  quote(do: x = x * x),
      ...>  quote(do: x = x - 1)
      ...> ] |> harnais_helper_test_forms(binding: [x: 7])
      {:ok, {63, ["x = x + 1", "x = x * x", "x = x - 1"]}}

      iex> [quote(do: x   = x + 1),
      ...>  quote(do: x = x   * x ),
      ...>  quote(do: x=x-1   )
      ...> ] |> harnais_helper_test_forms(binding: [x: 1])
      {:ok, {3, ["x = x + 1", "x = x * x", "x = x - 1"]}}
  """

  @since "0.1.0"

  @spec harnais_helper_test_forms(any, any) :: {:ok, {any, [String.t()]}} | {:error, error}

  def harnais_helper_test_forms(forms, opts \\ []) do
    with {:ok, {eval_value, text_forms, _reduced_form}} <-
           forms
           |> harnais_helper_eval_form(opts) do
      {:ok, {eval_value, text_forms}}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @since "0.1.0"

  @spec harnais_helper_test_forms!(any, any) :: {any, [String.t()]} | no_return

  def harnais_helper_test_forms!(forms, opts \\ []) do
    forms
    |> harnais_helper_test_forms(opts)
    |> case do
      {:ok, {result, text_forms}} -> {result, text_forms}
      {:error, error} -> raise error
    end
  end

  @doc false
  @since "0.1.0"

  def harnais_helper_test_forms_result_texts(forms, opts \\ []) do
    with {:ok, {result, text_forms, _reduced_form}} <- forms |> harnais_helper_test_forms(opts) do
      {:ok, {result, text_forms}}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc false
  @since "0.1.0"

  def harnais_helper_test_forms_texts(forms, opts \\ []) do
    with {:ok, {_result, text_forms, _reduced_form}} <- forms |> harnais_helper_test_forms(opts) do
      {:ok, text_forms}
    else
      {:error, %{__exception__: true}} = result -> result
    end
  end

  @doc false
  @since "0.1.0"

  defmacro __using__(_opts \\ []) do
    quote do
      import Harnais.Helper
    end
  end
end
