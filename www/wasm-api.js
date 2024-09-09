import * as utils from "./utils.js";

export class WasmApi {
    #isInitialized = false;
    constructor() {}

    async init(wasmPath) {
        const response = await fetch(wasmPath, {
            headers: {
                "Content-Type": "application/wasm",
            },
        });
        if (!response.ok) {
            throw new Error("Failed to fetch WASM");
        }
        const wasm = await WebAssembly.instantiate(await response.arrayBuffer(), {});
        const wasmInstance = wasm.instance;
        if (!wasmInstance.exports.init()) {
            throw new Error("Failed to initialize wff-helper");
        };

        this.wasmInstance = wasmInstance;
        this.exports = wasmInstance.exports;
        this.#isInitialized = true;
    }

    async waitUntilInitialized() {
        while (!this.#isInitialized) {
            await new Promise(r => setTimeout(r, 100));
        }
    }

    // Convenience function to prepare a typed byte array
    // from a pointer and a length into WASM memory.
    getView(ptr, len) {
        if (ptr <= 0) {
            console.error("AHHH");
        }
        return new Uint8Array(this.exports.memory.buffer, ptr, len);
    }

    // Returns a uint32 from 4 bytes
    getPtrFromBytes(ptr) {
        return new Uint32Array(this.exports.memory.buffer, ptr, 4)[0];
    }

    // JS strings are UTF-16 and have to be encoded into an
    // UTF-8 typed byte array in WASM memory.
    encodeString(string) {
        const capacity = string.length;// * 2 + 5; // As per MDN
        const ptr = utils.errorCheckPtr(this.exports.alloc(capacity));
        const { written } = new TextEncoder().encodeInto(string, this.getView(ptr, capacity));
        return [ptr, written, capacity];
    }

    encodeStringSlice(string) {
        const [ptr, len, _] = this.encodeString(string);
        if (len != string.length) {
            throw new Error("Failed to write string to memory")
        }
        return this.exports.makeStringSlice(ptr, len);
    }

    // Decode UTF-8 typed byte array in WASM memory into
    // UTF-16 JS string.
    decodeString(ptr, len) {
        if (len == 0) {
            console.log(ptr);
            return "";
        }
        return new TextDecoder().decode(this.getView(ptr, len));
    }

    decodeStringSlice(slicePtr) {
        const len = this.exports.getSliceLength(slicePtr);
        const ptr = this.exports.getSlicePtr(slicePtr);
        return this.decodeString(ptr, len);
    }

    decodeJsonStringSlice(slicePtr) {
        const string = this.decodeStringSlice(slicePtr);
        return JSON.parse(string);
    }
}