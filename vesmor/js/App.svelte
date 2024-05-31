<script lang="ts">
  import { onMount } from "svelte";
  import {
    get_code,
    set_output,
    load_code,
    set_vim_mode,
    init as init_editor,
  } from "./editor";
  import {
    full_reload,
    hot_reload,
    check_code,
    init as init_game,
  } from "./game_loader";

  import Manual from "./Manual.svelte";
  import Icons from "./Icons.svelte";
  import Modal from "./Modal.svelte";
  import StickyHeader from "./StickyHeader.svelte";
  import CloseDialogButton from "./CloseDialogButton.svelte";
  import Footer from "./Footer.svelte";

  function full_reload_() {
    let source = get_code();
    if (source != undefined && source != null) {
      full_reload(source).then(set_output);
    }
  }

  function hot_reload_() {
    let source = get_code();
    if (source != undefined && source != null) {
      hot_reload(source).then(set_output);
    }
  }

  function load_code_(code_location: string) {
    load_code(code_location).then((c) => {
      if (c) {
        full_reload_();
      }
    });
  }

  function check_source_code() {
    let source = get_code();
    if (source) {
      check_code(source).then((result) => {
        set_output(result);
      });
    }
  }

  let selected_code: string;

  let vim_mode: boolean = false;

  let showModal = true;
  onMount(() => {
    init_editor();
    init_game().then(full_reload_);
  });
</script>

<div class="main-box" id="main-box">
  <div class="vert-flex">
    <div id="codemirror-holder"></div>
    <p id="output"></p>
  </div>

  <div class="vert-flex btn-group">
    <button type="button" on:click={() => (showModal = !showModal)}
      ><Icons type={"book"} /> MANUAL
    </button>
    <hr />
    <button type="button" on:click={full_reload_}
      ><Icons type={"arrow-loop"} /> FULL RELOAD
    </button>
    <button type="button" on:click={hot_reload_}
      ><Icons type={"fire"} /> HOT RELOAD
    </button>
    <button type="button" on:click={check_source_code}
      ><Icons type={"check"} /> CHECK CODE
    </button>
    <hr />
    <select bind:value={selected_code}>
      <option value="pong.ves">Pong</option>
      <option value="bouncy_box.ves">Bouncy Box</option>
      <option value="triangle.ves">Triangle</option>
    </select>
    <button type="button" on:click={() => load_code_(selected_code)}
      ><Icons type={"download"} /> LOAD CODE
    </button>
    <hr />
    <label class="checkbox-container"
      >VIM MODE
      <input
        type="checkbox"
        bind:checked={vim_mode}
        on:change={() => {
          set_vim_mode(vim_mode);
        }}
      />
      <span class="checkmark"></span>
    </label>
    <!-- TODO: button for restarting the console (for when it crashes) -->
  </div>

  <div id="game-container">
    <div class="loader">
      <div class="lds-dual-ring"></div>
      <div class="loading-text">Loading...</div>
    </div>
  </div>
</div>

<Footer />

<Modal bind:showModal>
  <!-- 
    maybe manual should be open by default on first visit?
    would have to store that state in localstorage or smth
    or dont bother?
  -->
  <StickyHeader>
    <div id="modal-header">
      <h1 id="header-h1">Vesmor Manual</h1>
      <CloseDialogButton onclick={() => (showModal = false)} />
    </div>
  </StickyHeader>

  <Manual />
</Modal>

<style lang="scss">
  $highlight: #ff7d00;
  $highlight2: #fc7d5d;
  $dark-blue: #001020;
  #modal-header {
    background-color: $dark-blue;
    padding: 0.5em;
    border-bottom: 1px solid $highlight;

    display: flex;
    flex-wrap: wrap;
    text-align: center;
    justify-content: center;

    #header-h1 {
      padding-left: 3em;
      margin: 0;
      border: none;
      color: $highlight;
      width: auto;
      flex-grow: 1;
    }
  }

  .checkbox-container {
    color: $highlight;
    margin: 0.2rem;
    padding: 0.5rem 0.8rem;
    font-size: 1.3rem;
    display: block;
    position: relative;
    padding-left: 35px;
    margin-bottom: 12px;
    cursor: pointer;
    -webkit-user-select: none;
    -moz-user-select: none;
    -ms-user-select: none;
    user-select: none;

    input {
      position: absolute;
      opacity: 0;
      cursor: pointer;
      height: 0;
      width: 0;
    }
    .checkmark {
      position: absolute;
      top: 0.2rem;
      left: 0.2rem;
      height: 1.6rem;
      width: 1.6rem;
      background-color: $dark-blue;
      border: 2px solid $highlight;
    }
    &:hover input ~ .checkmark {
      background-color: #234;
    }
    & input:checked ~ .checkmark {
      background-color: $highlight;
    }
  }
</style>
