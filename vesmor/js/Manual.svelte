<script>
  import Code from "./Code.svelte";
</script>

<div id="manual">
  <h2>Overview</h2>
  <p>
    Vesmor is a
    <a href="https://en.wikipedia.org/wiki/Fantasy_video_game_console"
      >Fantasy Video Game Console</a
    >.
  </p>
  <p>
    It features a custom scripting language, called <Code>Vesmish</Code>, that
    is functional, declarative, statically typed and interpreted.
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
  <h4>Binary Operators</h4>
  <p>Binary operators take two values and return a new value.</p>
  <p>
    The use of a binary operator needs to be surrounded by parentheses. This
    makes it explicit and avoids ambiguity.
  </p>
  <p>
    For example:
    <br />
    <Code>(40 + 2)</Code>
    <br />
    or
    <br />
    <Code>(3 * (3 * 3))</Code>
    <br />
  </p>
  <p>Currently implemented operators:</p>
  <ul>
    <li>
      <Code>+ - * / %</Code> — mathematical operations on <Code>Int</Code>s
    </li>
    <li>
      <Code>&lt; &gt; ==</Code> — <Code>Int</Code> comparisons
    </li>
    <li>
      <Code>+. -. *. /.</Code> — mathematical operations on <Code>Float</Code>s
    </li>
    <li>
      <Code>&lt;. &gt;. ==.</Code> — <Code>Float</Code> comparisons
    </li>
    <li>
      <Code>~</Code> — <Code>String</Code> concatenation
    </li>
    <li>
      <Code>~~</Code> — <Code>List</Code> concatenation
    </li>
    <li>
      <Code>&amp;&amp; ||</Code> — <Code>Bool</Code> operations
    </li>
  </ul>
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
    <Code>(a: Int, b: Int) -&gt; Int (a+b)</Code>
    <br />
    This defines a simple function that just adds two
    <Code>Int</Code>s. We can assign it to
    <Code>add</Code>:
    <br />
    <Code>add = (a: Int, b: Int) -&gt; Int (a+b);</Code>
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
    <Code>None&grave;</Code>
    and
    <Code>Some&grave; *A</Code> (yes, variant names end with backticks).
    <br />
    <Code>Some&grave;</Code> can hold a value of any type (that's what
    <Code>*A</Code>
    means here).
    <br />
    They can be used like so:
    <br />
    <Code>foo = Option::None&grave;;</Code>
    <br />
    <Code>bar = Option::Some&grave; 5;</Code>
    <br />
  </p>
  <p>
    <Code>Bool</Code> is also an enum. It has two variants, which don't hold values:
    <Code>True&grave;</Code> and
    <Code>False&grave;</Code>.
  </p>
  <p>
    A nice feature that comes with
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
    <Code>? bar | Some&grave; v =&gt; v; | None&grave; =&gt; 5;</Code>
    <br />
    We are matching on the value of
    <Code>bar</Code>. Let's assume
    <Code>bar</Code> is of type
    <Code>Option&lt;Int&gt;</Code> (so the
    <Code>Some&grave;</Code> holds an
    <Code>Int</Code>, we'll elaborate on that later).
    <br />
    First branch matches
    <Code>Variant Some&grave;</Code> and returns the value inside.
    <br />
    Second branch matches
    <Code>Variant None&grave;</Code> and returns
    <Code>5</Code>.
    <br />
    As you can see, both branches return a value of the same type.
  </p>
  <p>
    You can also create a catch-all branch:
    <br />
    <Code>| x =&gt; &lbrace;...&rbrace;</Code>
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
    <Code>PlayerAction : | Jump&grave; | Move&grave; Float;</Code>
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
    return generic types in functions.
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
    When an event happens (such as key being pressed or game-tick happening),
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
    <Code>Command</Code>s are the way to interact with the console.
  </p>
  <p>
    <Code>Command</Code>s returned from <Code>event_handler</Code> will be executed
    for the next frame.
  </p>
  <p>
    Currently, the only <Code>Command</Code> provided is
    <Code>DrawLine</Code> command, which draws a straight line between two points.
    <br />
    It takes a value of type
    <Code>&lt; start: Vec2, end: Vec2 &gt;</Code>.
  </p>
  <h3>Game State</h3>
  <p>
    <Code>Game State</Code> contains all the data for your game. So that would include
    player score, positions of objects, etc.
  </p>
  <p>
    As mentioned, it is initialized to the value of <Code>init</Code> and modified
    by returning a different value from <Code>event_handler</Code>.
  </p>
  <h3>Events</h3>
  <p>Currently available <Code>Event</Code>s are:</p>
  <ul>
    <li>
      <Code>Tick` Float</Code> — fired on every tick/frame of the game. The value
      inside is time elapsed from last update.
    </li>
    <li>
      <Code>KeyPressed` Key</Code> — fired when key is pressed down (i.e. goes from
      being not pressed to being pressed).
      <Code>Key</Code> is an <Code>Enum</Code> with variants for keys A-Z.
    </li>
    <li>
      <Code>KeyReleased` Key</Code> — fired when key is released (i.g. goes from
      being pressed to not being pressed).
      <Code>Key</Code> is an <Code>Enum</Code> with variants for keys A-Z.
    </li>
    <li>
      <Code>KeyDown` Key</Code> — fired on every tick/frame for every key that is
      currently being held down.
      <Code>Key</Code> is an <Code>Enum</Code> with variants for keys A-Z.
    </li>
  </ul>
  <h2>Developing Games</h2>
  <p>
    This webpage includes the full devkit for developing games for the console.
  </p>
  <p>
    You can edit the code in the editor, reload it and play it in the embedded
    console!
  </p>
  <p>There are four buttons for interacting with the editor and console:</p>
  <ul>
    <li>
      <Code>Full Reload</Code> — checks the code for errors, loads it into the console
      and restarts the state. Useful when you make a change that impacts the initial
      state or makes the game not compatible with the previous state.
    </li>
    <li>
      <Code>Hot Reload</Code> — checks the code for errors, loads it into the console,
      but doesn't reload the state: the same state is used, preserving any changes.
      Useful when you make a change that doesn't break the state or you want to fix
      a specific interaction.
    </li>
    <li>
      <Code>Check Code</Code> — checks the code for errors, without loading it into
      the console. Useful when you're just messing around.
    </li>
    <li>
      <Code>Load Code</Code> — loads the selected example. <b>Warning:</b> this will
      overwrite the current code.
    </li>
  </ul>
  <p>
    You <i>may</i> encounter some console crashes. If it happens, you'll need to
    restart the console, by refreshing the webpage. Your code <i>should</i> be preserved.
  </p>
</div>
