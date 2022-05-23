/*eslint-env node*/
import "./static/css/main.css";
import * as Sentry from "@sentry/browser";
import { BrowserTracing } from "@sentry/tracing";

// We need to patch the JSON.stringify in order for BigInt serialization to work.
const { stringify, parse } = require("json-bigint")({ useNativeBigInt: true });

JSON.stringify = stringify;
JSON.parse = parse;

Sentry.init({
  dsn: process.env.SENTRY_DSN,
  integration: [new BrowserTracing()],
  tracesSampleRate: process.env.SENTRY_TRACES_SAMPLE_RATE,
  debug: process.env.NODE_ENV === "development",
  environment: process.env.NODE_ENV,
  release: process.env.SENTRY_RELEASE,
});

require("./output/Main").main({
  webpackDevelMode: process.env.NODE_ENV === "development",
  pollingInterval: parseInt(process.env.MARLOWE_POLLING_INTERVAL),
})();
