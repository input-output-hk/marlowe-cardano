const { stringify, parse } = require("json-bigint")({ useNativeBigInt: true });
import * as Sentry from "@sentry/browser";
import { sentryTransport } from "sentry-testkit";

JSON.stringify = stringify;
JSON.parse = parse;

Sentry.init({
  dsn: "https://acacaeaccacacacabcaacdacdacadaca@sentry.io/000001",
  release: "test",
  tracesSampleRate: 1,
  transport: sentryTransport,
});

require("../output/Test.Main").main();
