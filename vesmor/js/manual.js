import ManualPage from "./ManualPage.svelte";
import "./style.scss";

const app = new ManualPage({
  target: document.getElementById("root"),
});

export default app;
