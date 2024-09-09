import { WasmApi } from "./wasm-api.js";
import * as utils from "./utils.js";

document.addEventListener("DOMContentLoaded", fetchRules);

const wasmApi = new WasmApi();
await wasmApi.init("../zig-out/bin/wff-helper.wasm");

const proofPtr = await initPage();

document.getElementById("proof-start-button").addEventListener("click", () => utils.startProof(wasmApi));
document.getElementById("proof-wff-input").addEventListener("input", () => utils.updateProofMethods(wasmApi));
document.getElementById("button-check-step").addEventListener("click", checkStep);
document.getElementById("button-remove-step").addEventListener("click", removeLastStep);
document.getElementById("button-clear-proof").addEventListener("click", clearProof);
document.getElementById("input-wff-entry").addEventListener("keypress", event => {
    // If the user presses the "Enter" key on the keyboard
    if (event.key === "Enter") {
        // Cancel the default action, if needed
        event.preventDefault();
        // Trigger the button element with a click
        document.getElementById("button-check-step").click();
    }
});

window.addEventListener("beforeunload", function (e) {
    const stepsTable = document.getElementById("steps-table");
    if (stepsTable.lastChild) {
        e.preventDefault(); // If you prevent default behavior in Mozilla Firefox prompt will always be shown
    }
});

async function initPage() {
    await wasmApi.waitUntilInitialized();

    const urlParams = new URLSearchParams(window.location.search);
    const wffString = urlParams.get("wff");
    const proofMethod = urlParams.get("method");
    
    if (wffString.length == 0 || proofMethod.length == 0) {
        throw new Error("Empty or missing URL arg");
    }
    
    const wffStringPtr = utils.errorCheckPtr(wasmApi.encodeStringSlice(wffString));
    const wffPtr = utils.errorCheckPtr(wasmApi.exports.wffParse(wffStringPtr));
    const formattedWffSlicePtr = utils.errorCheckPtr(wasmApi.exports.wffGetString(wffPtr));
    const formattedWffString = wasmApi.decodeStringSlice(formattedWffSlicePtr);
    
    document.getElementById("proof-wff").textContent = formattedWffString;
    document.getElementById("proof-wff-input").value = formattedWffString;
    
    utils.updateProofMethods(wasmApi);
    
    document.getElementById("proof-method").textContent = utils.getDetailedProofMethodString(proofMethod);
    document.getElementById("proof-method-select").value = proofMethod;
    
    const proofMethodStringPtr = utils.errorCheckPtr(wasmApi.encodeStringSlice(proofMethod));
    const proofPtr_ = utils.errorCheckPtr(wasmApi.exports.proofInit(wffPtr, proofMethodStringPtr));
    
    // Add assumptions to the proof and populate them on the interface.
    const assumptions = urlParams.get("assumptions");
    if (assumptions) {
        if (assumptions.length == 0) {
            throw new Error("Empty or missing URL arg");
        }
        const formattedAssumptions = [];
        for (let assumptionString of assumptions.split("|")) {
            const assumptionStringSlicePtr = utils.errorCheckPtr(wasmApi.encodeStringSlice(assumptionString));
            const assumptionWffPtr = utils.errorCheckPtr(wasmApi.exports.wffParse(assumptionStringSlicePtr));
            utils.errorCheckInt(wasmApi.exports.proofAddAssumption(proofPtr_, assumptionWffPtr));

            const assumptionWffFormattedStringSlicePtr = utils.errorCheckPtr(wasmApi.exports.wffGetString(assumptionWffPtr));
            const assumptionWffFormattedString = wasmApi.decodeStringSlice(assumptionWffFormattedStringSlicePtr);
            formattedAssumptions.push(assumptionWffFormattedString);

            // wasmApi.exports.freeStringSlice(assumptionWffFormattedStringSlicePtr);
            // wasmApi.exports.wffFree(assumptionWffPtr);
            // wasmApi.exports.freeStringSlice(assumptionStringSlicePtr);
        }
        document.getElementById("assumptions").textContent = formattedAssumptions.join(", ");
    }
    
    wasmApi.exports.freeStringSlice(proofMethodStringPtr);
    // We don't deinit this, as it will be owned by the proof.
    wasmApi.exports.wffFree(wffPtr);
    wasmApi.exports.freeStringSlice(wffStringPtr);

    return proofPtr_
}

function fetchRules() {
    fetch("rules.json")
    .then(response => response.json())
    .then(data => populateRules(data))
    .catch(error => console.error("Error loading the rules JSON file:", error));
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
}

function checkStep() {
    const inputBox = document.getElementById("input-wff-entry");
    const input = inputBox.value.replace(/\s+/g, "");
    if (input.length == 0) {
        return;
    }
    
    // Try to parse. If invalid, display error
    const inputStringPtr = utils.errorCheckPtr(wasmApi.encodeStringSlice(input));
    
    // Check if valid step. If invalid, display error
    const stepPtr = utils.errorCheckPtr(wasmApi.exports.parseStep(inputStringPtr, proofPtr));
    const stepWffPtr = utils.errorCheckPtr(wasmApi.exports.proofStepGetWff(stepPtr));
    const stepWffSlicePtr = utils.errorCheckPtr(wasmApi.exports.wffGetString(stepWffPtr));
    const stepWffString = utils.errorCheckPtr(wasmApi.decodeStringSlice(stepWffSlicePtr));

    const justificationStringPtr = utils.errorCheckPtr(wasmApi.exports.proofStepGetJustificationString(stepPtr, proofPtr));
    const justificationString = wasmApi.decodeStringSlice(justificationStringPtr);

    if (utils.errorCheckInt(wasmApi.exports.proofStepIsValid(stepPtr))) {
        utils.errorCheckInt(wasmApi.exports.proofAddStep(proofPtr, stepPtr));
        addStep(stepWffString, justificationString);

        inputBox.value = null;

        if (utils.errorCheckInt(wasmApi.exports.proofIsFinished(proofPtr))) {
            completeProof();
        }
    } else {
        console.log("Invalid step");
    }
    
    wasmApi.exports.freeStringSlice(justificationStringPtr);
    wasmApi.exports.proofStepFree(stepPtr);
    wasmApi.exports.freeStringSlice(inputStringPtr);
}

function addStep(wffString, justificationString) {
    document.getElementById("button-remove-step").disabled = false;
    document.getElementById("button-clear-proof").disabled = false;
    const stepsTable = document.getElementById("steps-table");
    const tr = document.createElement("tr");
    tr.innerHTML = `<td>${wffString}</td><td class="steps-justification">${justificationString}</td>`
    stepsTable.appendChild(tr);
}

function removeLastStep() {
    const stepsTable = document.getElementById("steps-table");
    if (stepsTable.lastChild) {
        stepsTable.removeChild(stepsTable.lastChild);

        if (!stepsTable.lastChild) {
            document.getElementById("button-remove-step").disabled = true;
            document.getElementById("button-clear-proof").disabled = true;
        }
        return utils.errorCheckInt(wasmApi.exports.proofRemoveLastStep(proofPtr));
    }
    return false;
}

function completeProof() {
    // const provingWff = errorCheckPtr(exports.proofGetProvingWff(proofPtr));
    // const provingWffString = errorCheckPtr(exports.wffGetString(provingWff));
    document.getElementById("proof-conclusion").textContent = "QED";
    document.getElementById("input-wff-entry").disabled = true;
    document.getElementById("button-check-step").disabled = true;
    document.getElementById("button-remove-step").disabled = true;
    document.getElementById("bu tton-clear-proof").disabled = true;
}

function clearProof() {
    while (removeLastStep());
}