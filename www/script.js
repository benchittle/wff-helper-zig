// let alloc, free, parseWff, getProofMethods, getProofMethodString, proofInit, getByte;
let wasmInstance, memory, exports;
let proofPtr;

document.addEventListener("DOMContentLoaded", () => {
    fetch("rules.json")
    .then(response => response.json())
    .then(data => populateRules(data))
    .catch(error => console.error("Error loading the JSON file:", error));
});
document.addEventListener("DOMContentLoaded", () => {
    fetch("../zig-out/bin/wff-helper.wasm", {
        headers: {
            "Content-Type": "application/wasm",
        },
        
    }).then(response => response.arrayBuffer())
    .then(array => WebAssembly.instantiate(array, {}))
    .then(wasm => {
        wasmInstance = wasm.instance;
        memory = wasmInstance.exports.memory;
        exports = wasmInstance.exports;

        if (!wasmInstance.exports.init()) {
            throw new Error("Failed to initialize wff-helper");
        }

        const urlParams = new URLSearchParams(window.location.search);
        const wffString = urlParams.get("wff");
        const proofMethod = urlParams.get("method");

        const wffStringPtr = errorCheckPtr(encodeStringSlice(wffString));
        const wffPtr = errorCheckPtr(exports.wffParse(wffStringPtr));
        const formattedWffString = wffGetString(wffPtr);

        document.getElementById("proof-wff").textContent = formattedWffString;
        document.getElementById("proof-wff-input").value = formattedWffString;

        document.getElementById("proof-method").textContent = proofMethod;
        document.getElementById("proof-method-select").value = proofMethod;


        const proofMethodStringPtr = errorCheckPtr(encodeStringSlice(proofMethod));
        proofPtr = errorCheckPtr(exports.proofInit(wffPtr, proofMethodStringPtr));

        exports.freeStringSlice(proofMethodStringPtr);
            // exports.wffDeinit(wffPtr);
        exports.freeStringSlice(wffStringPtr);
        
    })
    .catch(error => console.error("Error loading the WASM:", error));
});


document.getElementById("input-wff-entry").addEventListener("keypress", wffEntryEventListener);
// document.getElementById("input-justification-entry").addEventListener("keypress", wffEntryEventListener);

function wffEntryEventListener(event) {
    // If the user presses the "Enter" key on the keyboard
    if (event.key === "Enter") {
        // Cancel the default action, if needed
        event.preventDefault();
        // Trigger the button element with a click
        document.getElementById("button-check-step").click();
    }
}

function startProof() {
    let wff = document.getElementById("proof-wff-input").value;
    wff = wff.replace(/\s+/g, "");
    if (wff.length == 0) {
        document.getElementById("home-error-text").hidden = false;
        console.log("Invalid wff");
        return;
    }
    let method = document.getElementById("proof-method-select").value;

    window.location.href = `proof.html?wff=${encodeURIComponent(wff)}&method=${encodeURIComponent(method)}` ;
}

function errorCheckPtr(ptr) {
    if (ptr == 0) {
        throw new Error("null error");
    }
    return ptr;
}

function errorCheckInt(int) {
    if (int < 0) {
        throw new Error("negative error");
    }
    return int;
}

// Convenience function to prepare a typed byte array
// from a pointer and a length into WASM memory.
function getView(ptr, len) {
    return new Uint8Array(memory.buffer, ptr, len);
}

// Returns a uint32 from 4 bytes
function getPtrFromBytes(ptr) {
    return new Uint32Array(memory.buffer, ptr, 4)[0];
}

// JS strings are UTF-16 and have to be encoded into an
// UTF-8 typed byte array in WASM memory.
function encodeStr(str) {
    const capacity = str.length;// * 2 + 5; // As per MDN
    const ptr = exports.alloc(capacity);
    const { written } = new TextEncoder().encodeInto(str, getView(ptr, capacity));
    return [ptr, written, capacity];
}

function encodeStringSlice(str) {
    const [ptr, len, _] = encodeStr(str);
    if (len != str.length) {
        throw new Error("Failed to write string to memory")
    }
    return exports.makeStringSlice(ptr, len);
}

// Decode UTF-8 typed byte array in WASM memory into
// UTF-16 JS string.
function decodeStr(ptr, len) {
    if (len == 0) {
        console.log(ptr);
        return "";
    }
    return new TextDecoder().decode(getView(ptr, len));
}

function decodeStrSlice(slicePtr) {
    const len = exports.getSliceLength(slicePtr);
    const ptr = exports.getSlicePtr(slicePtr);
    return decodeStr(ptr, len);
}

function decodeJsonStrSlice(slicePtr) {
    const str = decodeStrSlice(slicePtr);
    return JSON.parse(str);
}

function getAvailableProofMethods(wffStr) {
    const str = encodeStr(wffStr)[0];
    const wff = exports.wffParse(str, wffStr.length);
    exports.free(str);
    const methods = exports.proofGetAvailableMethods(wff);
    return methods;
}

function wffGetString(wffPtr) {
    const strSlicePtr = errorCheckPtr(exports.wffGetString(wffPtr));
    const str = decodeStrSlice(strSlicePtr);
    // exports.freeStringSlice(strSlicePtr);
    return str;
}

// Populates the equivalence and inference rules
function populateRules(data) {
    var equivalence_table = document.getElementById("equivalence-rules");
    data["equivalence_rules"].forEach(rule => {
        var tr = document.createElement("tr");
        tr.innerHTML = "<td>" + rule.name + "</td>" +
            "<td>" + rule.lhs + " ≡ " + rule.rhs + "</td>"
        equivalence_table.appendChild(tr);
    });

    var inference_table = document.getElementById("inference-rules");
    data["inference_rules"].forEach(rule => {
        var tr = document.createElement("tr");
        tr.innerHTML = "<td>" + rule.name + "</td>" +
            "<td><div class='fraction'><div class='numerator'>" + rule.conditions + "</div><div class='denominator'>" + rule.result + "</div></div></td>"
        inference_table.appendChild(tr);
    });

    // document.querySelectorAll(".resource").forEach(item => {
    //     item.style.flexBasis = "50px";
    // });
    // document.querySelectorAll(".resource").forEach(item => {
    //     item.style.flexBasis = "50px";
    // });
}

function checkStep() {
    const inputBox = document.getElementById("input-wff-entry");
    const input = inputBox.value.replace(/\s+/g, "");
    if (input.length == 0) {
        return;
    }
    // const inputJustificationBox = document.getElementById("input-justification-entry");
    // const inputJustification = inputJustificationBox.value;
    
    // Try to parse. If invalid, display error
    const inputStringPtr = errorCheckPtr(encodeStringSlice(input));
    
    // Check if valid step. If invalid, display error
    const stepPtr = errorCheckPtr(exports.parseStep(inputStringPtr, proofPtr));
    const stepWffPtr = errorCheckPtr(exports.proofStepGetWff(stepPtr));
    const stepWffString = errorCheckPtr(wffGetString(stepWffPtr));

    const justificationStringPtr = errorCheckPtr(exports.proofStepGetJustificationString(stepPtr, proofPtr));
    const justificationString = decodeStrSlice(justificationStringPtr);

    if (errorCheckInt(exports.proofStepIsValid(stepPtr))) {
        errorCheckInt(exports.proofAddStep(proofPtr, stepPtr));
        addStep(stepWffString, justificationString);

        inputBox.value = null;

        if (errorCheckInt(exports.proofIsFinished(proofPtr))) {
            completeProof();
        }
    } else {
        console.log("Invalid step");
    }
    
    exports.freeStringSlice(justificationStringPtr);
    exports.proofStepFree(stepPtr);
    exports.freeStringSlice(inputStringPtr);
}

function addStep(wffString, justificationString) {
    const stepsTable = document.getElementById("steps-table");
    const tr = document.createElement("tr");
    tr.innerHTML = `<td>${wffString}</td><td class="steps-justification">${justificationString}</td>`
    stepsTable.appendChild(tr);
}

function completeProof() {
    // const provingWff = errorCheckPtr(exports.proofGetProvingWff(proofPtr));
    // const provingWffString = errorCheckPtr(exports.wffGetString(provingWff));
    document.getElementById("proof-conclusion").textContent = "QED";
    document.getElementById("input-wff-entry").disabled = true;
    document.getElementById("button-check-step").disabled = true;
}