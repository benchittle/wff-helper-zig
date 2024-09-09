// Error Handling (WIP) //


export function errorCheckPtr(ptr) {
    if (ptr <= 0) {
        throw new Error("null error");
    }
    return ptr;
}

export function errorCheckInt(int) {
    if (int < 0) {
        throw new Error("negative error");
    }
    return int;
}



// Misc //


export function isValidWff(wasmApi, wffString) {
    const slicePtr = errorCheckPtr(wasmApi.encodeStringSlice(wffString));
    let isValid = true;
    try {
        const wffPtr = errorCheckPtr(wasmApi.exports.wffParse(slicePtr));
        wasmApi.exports.wffDeinit(wffPtr);
    } catch(err) {
        isValid = false;
    }
    wasmApi.exports.freeStringSlice(slicePtr);
    return isValid;
}


// Header functions //
// (These should probably be moved elsewhere since they aren't general utilities)


export function startProof(wasmApi) {
    let wff = document.getElementById("proof-wff-input").value;
    wff = wff.replace(/\s+/g, "");
    if (wff.length == 0) {
        document.getElementById("home-error-text").hidden = false;
        console.log("Invalid wff");
        return;
    }
    const method = document.getElementById("proof-method-select").value;
    
    const assumptionsList = [];
    if (document.getElementById("assumptions-input")) {
        const assumptionsText = document.getElementById("assumptions-input").value;
        if (assumptionsText.replace(/\s+/g, "").length > 0) {

            for (let line of assumptionsText.split(/\r?\n/)) {
                if (!isValidWff(wasmApi, line)) {
                    console.log("invalid assumption wff");
                    return;
                };
                assumptionsList.push(line.replace(/\s+/g, ""));
            }
        }
    }
    if (assumptionsList.length > 0) {
        const assumptionsString = assumptionsList.join("|");
        window.location.href = `proof.html?wff=${encodeURIComponent(wff)}&method=${encodeURIComponent(method)}&assumptions=${encodeURIComponent(assumptionsString)}`;
    } else {
        window.location.href = `proof.html?wff=${encodeURIComponent(wff)}&method=${encodeURIComponent(method)}`;
    }
}

export function updateProofMethods(wasmApi) {
    const wffText = document.getElementById("proof-wff-input").value;
    if (wffText.replace(/\s+/g, "").length == 0) {
        return;
    }
    const wffStringPtr = errorCheckPtr(wasmApi.encodeStringSlice(wffText));
    
    let wffPtr;
    try {
        wffPtr = errorCheckPtr(wasmApi.exports.wffParse(wffStringPtr));
    } catch(err) {
        return;
    }

    const proofMethodsJsonPtr = errorCheckPtr(wasmApi.exports.getAvailableProofMethodsJson(wffPtr));
    const proofMethodsJson = wasmApi.decodeJsonStringSlice(proofMethodsJsonPtr);
    
    const select = document.getElementById("proof-method-select");
    const oldSelection = select.value;
    select.replaceChildren();
    for (let method of proofMethodsJson) {
        let option = document.createElement("option");
        option.value = method.toLowerCase();
        option.textContent = getDetailedProofMethodString(method);
        select.appendChild(option);
    }
    if (proofMethodsJson.includes(oldSelection)) {
        select.value = oldSelection;
    }
}

export function getDetailedProofMethodString(proofMethod) {
    proofMethod = proofMethod.toLowerCase();
    if (proofMethod === "none") {
        return "No Proof Method";
    } else if (proofMethod === "direct") {
        return "Direct Proof";
    } else if (proofMethod === "indirect") {
        return "Indirect Proof";
    } else if (proofMethod === "contradiction") {
        return "Proof by Contradiction";
    } else {
        return "Unknown proof method";
    }
}