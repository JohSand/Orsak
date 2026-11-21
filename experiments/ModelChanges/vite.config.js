// Serves the app, compiled by Fable into out/, and sends its remote calls to the remoting sample's server.
import { defineConfig } from "vite";

export default defineConfig({
  server: {
    port: 5173,
    // dotnet writes obj/ and bin/ while Vite watches, which fails on Windows when a file is locked
    watch: { ignored: ["**/obj/**", "**/bin/**", "**/tests-out/**"] },
    proxy: { "/api": "http://localhost:5288" },
  },
});
