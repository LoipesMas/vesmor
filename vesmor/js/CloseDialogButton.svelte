<script>
  import Icons from "./Icons.svelte";

  export let onclick;
</script>

<button on:click={onclick}>
  <Icons type="x" />
  CLOSE
</button>
