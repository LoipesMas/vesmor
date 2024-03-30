import("../pkg/index.js").then(module =>{
    //TODO: pass source code here somehow
   module.main_web();
   window.update_source_code = module.update_source_code;
});

function full_reload() {
    let source = document.getElementById("sourceText").value;
    update_source_code(source, false).then(result => {
        console.log(result)
    })
}
function hot_reload() {
    let source = document.getElementById("sourceText").value;
    update_source_code(source, true).then(result => {
        console.log(result)
    })
}
window.full_reload = full_reload;
window.hot_reload = hot_reload;
