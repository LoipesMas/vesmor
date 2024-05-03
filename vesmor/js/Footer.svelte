<script>
  import Icons from "./Icons.svelte";
</script>

<footer>
  <div>
    <div>Created by LoipesMas</div>
    <div>
      <a href="https://github.com/LoipesMas/vesmor"
        ><Icons type="gh" /> Source Code</a
      >
    </div>
  </div>
</footer>

<style lang="scss">
  $highlight: #ff7d00;
  footer {
    display: flex;
    justify-content: center;
    color: #ff7d00;
    font-size: 1.2em;
    position: absolute;
    bottom: 0;
    margin: 1em;
    text-align: center;
    width: calc(100% - 2em);

    & > div {
      margin: 0.5em;
    }
  }
</style>
