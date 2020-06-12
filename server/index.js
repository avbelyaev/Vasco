// created by avbelyaev on 12.06.2020


const WAT_FILE_NAME = "module.wat";
const WASM_FILE_NAME = "module.wasm";


getRunArgs = () => {
    const runArgsElem = document.getElementById("runArgs");
    return runArgsElem.value;
}

log = (text) => {
    const loggerElem = document.getElementsByClassName("logger")[0];
    const loggerContentsElem = loggerElem.getElementsByClassName("logger-content")[0];

    const now = new Date();
    const currentTime = `${now.getHours()}:${now.getMinutes()}:${now.getSeconds()}`;
    const msg = `${currentTime}\t\t${getRunArgs()}\t=> ${text}<br/>`;

    loggerContentsElem.innerHTML += msg;
    console.log(msg);

    // scroll log to the bottom
    loggerContentsElem.scrollTop = loggerContentsElem.scrollHeight;
}

run = () => {
    WebAssembly.instantiateStreaming(fetch(WASM_FILE_NAME))
        .then(obj => {
            eval(`
                const { dummy } = obj.instance.exports;
                const result = dummy(${getRunArgs()});
                log(result);
            `)
        });
}
