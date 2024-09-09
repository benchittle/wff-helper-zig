import { WasmApi } from "./wasm-api.js";
import * as utils from "./utils.js";


const wasmApi = new WasmApi();
await wasmApi.init("./bin/wff-helper.wasm");
utils.updateProofMethods(wasmApi);

document.getElementById("proof-start-button").addEventListener("click", () => utils.startProof(wasmApi));
document.getElementById("proof-wff-input").addEventListener("input", () => utils.updateProofMethods(wasmApi));