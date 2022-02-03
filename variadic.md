# Variadic Functions
[![Run in Livebook](https://livebook.dev/badge/v1/blue.svg)](https://livebook.dev/run?url=https%3A%2F%2Fgithub.com%2Fbrettbeatty%2Fexperiments_elixir%2Fblob%2Fmaster%2Fvariadic.livemd)

## Macro

I recently discovered Elixir has a `...` operator I couldn't find in the docs, so I did the
only logical thing: take a crack at implementing variadic functions. They're really quite
useless--as with many of my experiments the fun is in trying to do crazy things with the
language.

In thinking how I would tackle the implementation I came up with three ideas for solutions:

* **Handle undefined functions.** Elixir has a lesser-known feature where you can define a
  `$handle_undefined_function/2` function for a module that handles remote calls to functions
  in the module that don't exist.

  That works, and it seemed like the easiest approach, but it has one major drawback:
  functions "defined" that way don't actually exist. It handles remote calls fine, which is
  what I want 99% of the time, but sometimes you want your functions to exist (when
  importing, for example).

* **Define all possible arities.** Function arities have a hard cap at 255 (I've never seen
  a function get close to that). With a finite set of arities one solution to implementing
  variadic functions would be to define them all.

  This appraoch is a little crazy, and it generates a lot of functions that will go unused,
  but each function exists.

* **Define arities on demand.** This idea is similar to the last, but instead of defining
  functions for every possible arity what if there was a way to only define the ones we care
  about, the ones that actually get used.

  This path would have seemed the most exciting to me, but it would have required collecting
  data across the project then recompiling modules that define variadic functions. I think it
  could have been aided by my [Index](https://github.com/brettbeatty/index_elixir) project,
  but I decided not to go this route.

Ultimately I decided to go the "define all possible arities" route. I created a module with
its own `def` and `defp` macros. If they're used to define a function with a `...` in the
args, they define a number of functions, rather than just one:

* A minimum-arity variation includes all the defaults provided when calling the macro so I
  wouldn't have to worry about re-implementing the logic for default arguments. It calls the
  implementation with an empty list for the variadic argument.
* Higher arities are defined recursively, up to the max. Each iteration adds another variable
  to both the args and the list passed to the implementation.
* A private implementation function (same name as variadic function but with `VARIADIC-`
  prepended) is started with the definition passed to the macro, if any.

Subsequent clauses of the variadic function get intercepted and transformed into additional
clauses of the private implementation function. That's tracked via module attribute.

Anyway, it's not the prettiest code I've written, but it seems to get the job done.

```elixir
defmodule Variadic do
  @typep builder() :: (module(), :def | :defp, keyword() | nil -> Macro.t())

  defmacro def(call, expr \\ nil) do
    defv(__CALLER__, :def, call, expr)
  end

  defmacro defp(call, expr \\ nil) do
    defv(__CALLER__, :defp, call, expr)
  end

  @spec defv(Macro.Env.t(), :def | :defp, Macro.t(), keyword() | nil) :: Macro.t()
  defp defv(%Macro.Env{module: module}, kind, call, expr) do
    with :error <- check_variadic(call),
         :error <- check_impl(module, call) do
      quote do
        Kernel.def(unquote(call), unquote(expr))
      end
    else
      {:ok, build} ->
        build.(module, kind, expr)
    end
  end

  @spec check_variadic(Macro.t()) :: {:ok, builder()} | :error
  defp check_variadic(call) do
    with {:ok, name, args} <- check_call(call) do
      case Enum.split_while(args, &not_spread?/1) do
        {prev, [{:..., _meta, [var]} | next]} ->
          {:ok, &build_variadic(&1, &2, name, prev, var, next, &3)}

        {_args, []} ->
          :error
      end
    end
  end

  @spec check_call(Macro.t()) :: {:ok, atom(), [Macro.t()]} | :error
  defp check_call(call)

  defp check_call({name, _meta, args}) when is_atom(name) and is_list(args) do
    {:ok, name, args}
  end

  defp check_call(_call) do
    :error
  end

  @spec not_spread?(Macro.t()) :: boolean()
  defp not_spread?(arg) do
    not match?({:..., _, [{name, _, ctx}]} when is_atom(name) and is_atom(ctx), arg)
  end

  @spec build_variadic(
          module(),
          :def | :defp,
          atom(),
          [Macro.t()],
          Macro.t(),
          [Macro.t()],
          keyword() | nil
        ) :: Macro.t()
  defp build_variadic(module, kind, name, prev, var, next, expr) do
    impl_name = impl_name(name)
    impl_arity = length(prev) + length(next) + 1
    register(module, name, impl_arity)

    # raw meaning "with defaults"
    prev_raw = prev
    next_raw = next
    prev = Enum.map(prev, &without_defaults/1)
    next = Enum.map(next, &without_defaults/1)

    acc = [
      build_variation(kind, name, prev_raw, next_raw, impl_name, prev, [], next),
      build_impl(module, kind, impl_name, prev ++ [var | next], expr)
    ]

    build = &build_variation(kind, name, prev, &1, impl_name, prev, &2, next)

    buildv(1, 256 - impl_arity, build, next, [], elem(var, 0), acc)
  end

  @spec register(module(), atom(), arity()) :: :ok
  defp register(module, name, impl_arity) do
    unless Module.has_attribute?(module, :variadic) do
      Module.register_attribute(module, :variadic, accumulate: true)
    end

    Module.put_attribute(module, :variadic, {name, impl_arity})
  end

  @spec without_defaults(Macro.t()) :: Macro.t()
  defp without_defaults(arg) do
    with {:\\, _meta, [arg, _default]} <- arg do
      arg
    end
  end

  @spec build_variation(
          :def | :defp,
          atom(),
          [Macro.t()],
          [Macro.t()],
          atom(),
          [Macro.t()],
          [Macro.t()],
          [Macro.t()]
        ) :: Macro.t()
  defp build_variation(kind, name, prev_args, args, impl_name, prev, chunk, next) do
    quote do
      Kernel.unquote(kind)(unquote(name)(unquote_splicing(prev_args ++ args))) do
        unquote(impl_name)(unquote_splicing(prev ++ [chunk | next]))
      end
    end
  end

  @spec buildv(
          non_neg_integer(),
          non_neg_integer(),
          ([Macro.t()], [Macro.t()] -> Macro.t()),
          [Macro.t()],
          [Macro.t()],
          atom(),
          [Macro.t()]
        ) :: Macro.t()
  defp buildv(count, total, build, args, chunk, var_name, acc)

  defp buildv(count, total, build, args, chunk, var_name, acc) when count <= total do
    var = {var_name, [counter: count], __MODULE__}
    args = [var | args]
    chunk = [var | chunk]

    variation = build.(args, chunk)

    buildv(count + 1, total, build, args, chunk, var_name, [variation | acc])
  end

  defp buildv(_count, _total, _build, _args, _chunk, _var_name, acc) do
    quote do
      (unquote_splicing(acc))
    end
  end

  @spec check_impl(module(), Macro.t()) :: {:ok, builder()} | :error
  defp check_impl(module, call) do
    with {:ok, name, args} <- check_call(call) do
      arity = length(args)

      if {name, arity} in Module.get_attribute(module, :variadic, []) do
        {:ok, &build_impl(&1, &2, impl_name(name), args, &3)}
      else
        :error
      end
    end
  end

  @spec impl_name(atom()) :: atom()
  defp impl_name(name) do
    :"VARIADIC-#{name}"
  end

  @spec build_impl(module(), :def | :defp, atom(), [Macro.t()], keyword() | nil) :: Macro.t()
  defp build_impl(_module, _kind, impl_name, args, expr) do
    quote do
      Kernel.defp(unquote(impl_name)(unquote_splicing(args)), unquote(expr))
    end
  end
end
```

## Usage

This is kind of a contrived example, but it shows off a few features I wanted to highlight:

* using `...`
* default argument
* multiple clauses
* matching on args

```elixir
defmodule MyApp do
  import Kernel, except: [def: 1, def: 2], warn: false
  import Variadic, only: [def: 1, def: 2], warn: false

  def do_something(range \\ 1..5, ...(values))

  def do_something(range = _.._//1, values) do
    Enum.reduce(values, range, fn value, first..last ->
      min(first, value)..max(last, value)
    end)
  end

  def do_something(_range, _values) do
    raise "ranges with step other than 1 not supported"
  end
end
```

If no arguments are passed to the function, it uses the default for `range`.

```elixir
MyApp.do_something()
```

If one argument is passed, it is used to override the default for `range`.

```elixir
MyApp.do_something(2..4)
```

Subsequent values are passed in a list to `values`. Values inside the range are ignored.

```elixir
MyApp.do_something(2..4, 4)
```

Whereas values outside the range expand its bounds as necessary.

```elixir
MyApp.do_something(2..4, 4, 20, -4)
```
