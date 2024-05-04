<script lang="ts">
  import { onMount } from "svelte";
  import {
    get_code,
    set_output,
    reset_code,
    DEFAULT_CODE_LOCATION,
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

  function reset_code_(code_location: string) {
    reset_code(code_location).then((c) => {
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

  let showModal = true;
  onMount(() => {
    init_game();
    init_editor();
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
    </select>
    <button type="button" on:click={() => reset_code_(selected_code)}
      ><Icons type={"trash"} /> LOAD CODE
    </button>
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
</style>
