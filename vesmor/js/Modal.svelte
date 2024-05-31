<script lang="ts">
  export let showModal: boolean;

  let modalVisible: boolean;

  let dialog: HTMLDialogElement | null;

  $: if (dialog && showModal) {
    modalVisible = true;
    dialog.showModal();
  }
  $: if (dialog && !showModal) {
    close();
  }

  function close() {
    modalVisible = false;
    showModal = false;
    setTimeout(() => {
      if (dialog) dialog.close();
    }, 400);
  }
</script>

<!-- svelte-ignore a11y-click-events-have-key-events a11y-no-noninteractive-element-interactions -->
<dialog
  bind:this={dialog}
  class:show={modalVisible}
  on:click|self={close}
  on:close={() => (showModal = false)}
  on:keydown|preventDefault={(event) => {
    if (event.key === "Escape") {
      close();
    }
  }}
>
  <!-- svelte-ignore a11y-no-static-element-interactions -->
  <div on:click|stopPropagation>
    <slot />
  </div>
</dialog>

<style lang="scss">
  $dark-blue: #001020;
  $highlight: #ff7d00;
  dialog {
    background-color: $dark-blue;
    border: 1px solid $highlight;
    margin: auto;
    padding: 0em;
    width: 85vh;
    max-width: 90%;
    text-align: center;
    font-size: 1.3em;
    height: 90vh;

    top: -100%;
    opacity: 0;
    transition:
      top 0.4s,
      opacity 0.4s;
  }
  dialog.show {
    /* transform: translate(0px, 0%); */
    top: 0;
    opacity: 1;
  }
  dialog::backdrop {
    background: rgba(0, 0, 0, 0.3);
  }
  /* dialog[open] { */
  /*     animation: zoom 0.5s cubic-bezier(0.34, 0.56, 0.64, 1); */
  /* } */
  @keyframes zoom {
    from {
      transform: translate(0px, -30%);
      opacity: 0;
    }
    to {
      transform: translate(0px, 0%);
      opacity: 1;
    }
  }
  dialog[open]::backdrop {
    animation: fade 0.2s ease-out;
  }
  @keyframes fade {
    from {
      opacity: 0;
    }
    to {
      opacity: 1;
    }
  }
</style>
