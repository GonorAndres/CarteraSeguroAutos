const BACKEND_ORIGIN = "https://cartera-autos-d3qj5vwxtq-uc.a.run.app";

export default {
  async fetch(request) {
    const incomingUrl = new URL(request.url);
    const backendUrl = new URL(incomingUrl.pathname + incomingUrl.search, BACKEND_ORIGIN);

    // Pages is the public edge. Cloud Run remains the application origin and
    // continues to handle Shiny HTTP requests, sessions, and WebSockets.
    return fetch(new Request(backendUrl, request));
  },
};
