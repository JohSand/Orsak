// Fable.Remoting's client sends with XMLHttpRequest, which browsers have and Node.js doesn't: xhr2 provides it.
import XMLHttpRequest from "xhr2";
globalThis.XMLHttpRequest = XMLHttpRequest;
await import("./out/Program.js");
