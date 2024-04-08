import("../pkg/index.js").then(module =>{
   module.main_web();
   window.update_source_code = module.update_source_code;
   window.check_code = module.check_code;
   window.full_reload();
});

function set_output(t) {
    const e = document.getElementById("output");
    e.innerText = t.slice(0,500);
}

function full_reload() {
    let source = document.getElementById("sourceText").value;
    update_source_code(source, false).then(result => {
        set_output(result)
    })
}
function hot_reload() {
    let source = document.getElementById("sourceText").value;
    update_source_code(source, true).then(result => {
        set_output(result)
    })
}
window.full_reload = full_reload;
window.hot_reload = hot_reload;


// load default source code
function reset_code() {
    fetch("game.vvc").then(resp => resp.text()).then(text => {
        const input = document.getElementById("sourceText")
        input.value = text;
        full_reload();
    })
}
window.reset_code = reset_code;

function check_source_code() {
    let source = document.getElementById("sourceText").value;
    check_code(source, true).then(result => {
        set_output(result)
    })
}
window.check_source_code = check_source_code;

fetch("game.vvc").then(resp => resp.text()).then(text => {
    const input = document.getElementById("sourceText")
    if (input.value === "") {
        input.value = text;
    }
})

