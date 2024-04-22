<script>
  import Code from "./Code.svelte";
</script>

<div id="manual">
  <h2>Overview</h2>
  <p>
    VVC is a
    <a href="https://en.wikipedia.org/wiki/Fantasy_video_game_console"
      >Fantasy Video Game Console</a
    >.
  </p>
  <p>
    It features a custom scripting language, that is functional, declarative,
    statically typed and interpreted.
  </p>
  <p>
    The limitation of this console is that it can only draw
    <a href="https://en.wikipedia.org/wiki/Vector_graphics">vector graphics</a>
    (currently only lines, maybe more to come).
  </p>
  <h2>Language</h2>
  <p>
    As mentioned, the scripting language is a custom one. It's inspired by
    <a href="https://elm-lang.org/">Elm</a>,
    <a href="https://www.roc-lang.org/">Roc</a> and
    <a href="https://nixos.org/manual/nix/stable/language/index.html">Nix</a>.
    <br />
  </p>
  <p>It's features include:</p>

  <ul>
    <li>functional and pure</li>
    <li>declarative</li>
    <li>statically typed</li>
    <li>tagged unions</li>
    <li>no side-effects</li>
    <li>hot-reloadable</li>
    <li>interpreted</li>
    <li>curry-ing by default</li>
  </ul>
  <p>Syntax is mostly similar to Rust, but there are some differences.</p>
  <h3>Basic concepts</h3>
  <h4>Expressions</h4>
  <p>
    Everything is an expression. Since you can't have side-effects or mutation,
    things only matter if they are assigned to something.
  </p>
  <p>
    Because of that, everything evaluates to something. Functions must return
    something, all branches must return a value, etc.
  </p>
  <h4>Definitions</h4>
  <p>
    Definitions are just assigning an expression to a name. For example:
    <br />
    <Code>x = 5;</Code>
    <br />
    This will assign
    <Code>5</Code> to <Code>x</Code>;
  </p>
  <p>All definitions need to end with a semicolon.</p>
  <p>You can't redefine/shadow definitions!</p>
  <p>
    <b>Note:</b> currently, order of definitions can
    <i>sometimes</i> matter.
    <br />
    This is an interpreter bug and will be fixed one day.
  </p>
  <p><u>A file is just a collection of definitions.</u></p>
  <h4>Basic types</h4>
  <p>
    The most basic types are <Code>Int</Code>,
    <Code>Float</Code>
    and
    <Code>String</Code>. They are what you expect (although
    <Code>String</Code>s are pretty useless for now).
  </p>
  <p>Those types are the building blocks of other types.</p>
  <p>
    Important thing of note is that you can't mix
    <Code>Float</Code>s and
    <Code>Int</Code>s. They have separate operators (like in OCaml).
  </p>
  <h4>Functions</h4>
  <p>
    You can think of functions as just another type. It's a very special type
    which can be called. When called with all the required arguments, it will
    evaluate to a value.
  </p>
  <p>
    Function definition consists of arguments it takes, return type and body.
    Arguments are names with types, return type is the type of the returned
    value and the body is an expression that has arguments in scope and
    evaluates to a value of the return type. Example:
    <br />
    <Code>(a: Int, b: Int) -> Int (a+b)</Code>
    <br />
    This defines a simple function that just adds two
    <Code>Int</Code>s. We can assign it to
    <Code>add</Code>:
    <br />
    <Code>add = (a: Int, b: Int) -> Int (a+b);</Code>
    <br />
    and then call it:
    <br />
    <Code>add(40,2)</Code>
    <br />
  </p>
  <h4>Enums and Enum Matching</h4>
  <p>
    Another important "group" of types are
    <Code>Enum</Code>s.
    <br />
    <Code>Enum</Code>s here are a form of
    <a href="https://en.wikipedia.org/wiki/Tagged_union">tagged unions</a>.
    <br />
    This means that
    <Code>Enum Variants</Code> can be defined to hold a value.
  </p>
  <p>
    One example of an
    <Code>Enum</Code>
    is
    <Code>Option</Code>.
    <br />
    It has two possible variants:
    <Code>None&#96;</Code>
    and
    <Code>Some&#96; *A</Code> (yes, variant names end with backticks).
    <br />
    <Code>Some&#96;</Code> can hold a value of any type (that's what
    <Code>*A</Code>
    means here).
    <br />
    They can be used like so:
    <br />
    <Code>foo = Option::None&#96;;</Code>
    <br />
    <Code>bar = Option::Some&#96; 5;</Code>
    <br />
  </p>
  <p>
    <Code>Bool</Code> is also an enum. It has two variants, which don't hold values:
    <Code>True&#96;</Code> and
    <Code>False&#96;</Code>.
  </p>
  <p>
    Very nice feature that comes with
    <Code>Enum</Code>s is
    <Code>Enum Matching</Code>.
    <br />
    It is a subset of pattern matching found in other languages. With that, you can
    have conditional logic, based on the
    <Code>Enum Variant</Code>.
    <br />
  </p>
  <p>
    Let's see an example:
    <br />
    <Code>? bar | Some&#96; v => v; | None&#96; => 5;</Code>
    <br />
    We are matching on the value of
    <Code>bar</Code>. Let's assume
    <Code>bar</Code> is of type
    <Code>Option&lt;Int&gt;</Code> (so the
    <Code>Some&#96;</Code> holds an
    <Code>Int</Code>, we'll elaborate on that later).
    <br />
    First branch matches
    <Code>Variant Some&#96;</Code> and returns the value inside.
    <br />
    Second branch matches
    <Code>Variant None&#96;</Code> and returns
    <Code>5</Code>.
    <br />
    As you can see, both branches return a value of the same type.
  </p>
  <p>
    You can also create a catch-all branch:
    <br />
    <Code>| x => &#123;...};</Code>
    <br />
    This branch will match any value and
    <Code>x</Code> will have the value of the expression which you were matching
    on (so in previous example that would be the value of
    <Code>bar</Code>
    ).
  </p>
  <p>
    <Code>Enum Matching</Code> can be used with
    <Code>Bool</Code> as if-else-statements.
  </p>
  <p>
    Here's how you can define your own
    <Code>Enum</Code> type:
    <br />
    <Code>PlayerAction : | Jump&#96; | Move&#96; Float;</Code>
    <br />
  </p>
  <p>
    Note: currently it's not possible to define
    <Code>Enum</Code>s that are generic over the value they hold.
  </p>
  <h4>Records</h4>
  <p>
    <Code>Record</Code>s are objects that can hold multiple values of different
    types, which can be accessed by name.
    <br />
    <Code>user = &lt; name = "John"; age = 42;&gt;;</Code>
    <br />
    You can access the member like so:
    <br />
    <Code>@user.name</Code>
    <br />
    It also works for nested records:
    <br />
    <Code>@@player.speed.x</Code>
    <br />
    (Yes, you need a
    <Code>@</Code>
    for every level of access. It's a bit unwieldy, but it's a limitation of the
    current parser. It will be addressed one day. )
  </p>
  <p>
    You will also need to define your own
    <Code>Record Type</Code>s. Here's how it would look like for the
    <Code>user</Code>:
    <br />
    <Code>User : &lt; name: String, age: Int &gt;;</Code>
    <br />
  </p>
  <h4>Lists</h4>
  <p>
    Final type on our list (pun intended) is
    <Code>List</Code>.
    <br />
    Syntax for creating them looks like this:
    <br />
    <Code>scores = [1,5,16234];</Code>
    <br />
  </p>
  <p>Functions for working with lists are:</p>
  <ul>
    <li>
      <Code>list_size: (List&lt;*A&gt;) -&gt; Int</Code>
      — for getting the size of the list
    </li>
    <li>
      <Code>list_get: (List&lt;*A&gt;, Int) -&gt; Option&lt;*A&gt;</Code>
      — for getting an item from the given index
    </li>
    <li>
      <Code>list_map: (List&lt;*A&gt;, (*A) -&gt; *B) -&gt; List&lt;*B&gt;</Code
      >
      — for transforming the list with the given function
    </li>
    <li>
      <Code>list_fold: (List&lt;*A&gt;, (*B,*A) -&gt; *B, *B) -&gt; *B</Code>
      — for reducing the list to a single value
    </li>
  </ul>
  <h4>Generic Types</h4>
  <p>
    Some of the builtin types are generic over a type. This means that their
    definition doesn't specify what the type of some inner value will be.
    Instead, this will be declared by you or inferred from usage.
  </p>
  <p>
    For example, to use a list of integers in a type definition, you would do it
    like this:
    <br />
    <Code>Game : &lt; scores: List&lt;Int&gt; &gt;;</Code>
    <br />
  </p>
  <p>
    Right now it's not possible to define your own generic types; or take or
    return generic types.
  </p>
  <h2>Game Loop</h2>
  <p>
    The architecture is similar to Elm's "Model-View-Update", but view and
    update are merged.
  </p>
  <p>The game loop looks roughly like this:</p>
  <p>
    When the game starts, the
    <Code>Game State</Code>
    is initialized to the <Code>init</Code> value.
  </p>
  <p>
    On every frame <Code>update_handler</Code> function is called. It receives current
    <Code>Game State</Code>
    and time delta; and returns new
    <Code>Game State</Code>
    and <Code>Commands</Code>.
  </p>
  <p>
    When an event happens (such as key being pressed)
    <Code>event_handler</Code> function is called. It receives current
    <Code>Game State</Code>
    and the
    <Code>Event</Code>; and returns new
    <Code>Game State</Code>
    and <Code>Commands</Code>.
  </p>
  <p>
    <Code>Game State</Code> can only be modified by changing the returned value.
    Visuals can be only drawn through the returned
    <Code>Commands</Code>.
  </p>
  <h3>Commands</h3>
  <p>
    <Code>Commands</Code> are the way of interacting with the console.
  </p>
  <p>
    Currently, the only command provided is
    <Code>DrawLine</Code> command, which draws a straight line between two points.
    <br />
    It takes a value of type
    <Code>&lt; start: Vec2, end: Vec2 &gt;</Code>.
  </p>

  <h3 id="game-state">Game State</h3>
  <h3>Update</h3>
  <h3>Events</h3>
  <h2>Developing Games</h2>
  <p>
    TODO: describe how to make games: writing code, checking code, hot/full
    reloads, sharing code
  </p>
</div>
