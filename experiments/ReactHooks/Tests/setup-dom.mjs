// A DOM for React on Node.js, set up before the tests import React: node --import ./Tests/setup-dom.mjs
import { JSDOM } from "jsdom";

const dom = new JSDOM("<!doctype html><html><body></body></html>", { url: "http://localhost/" });
for (const name of ["window", "document", "Node", "HTMLElement", "HTMLIFrameElement", "Event", "MouseEvent"]) {
  globalThis[name] = name === "window" ? dom.window : dom.window[name];
}
// React's act warns unless it knows it runs in tests
globalThis.IS_REACT_ACT_ENVIRONMENT = true;
