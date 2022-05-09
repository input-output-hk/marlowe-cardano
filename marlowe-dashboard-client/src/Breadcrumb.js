const Sentry = require("@sentry/browser");

exports._addBreadcrumb = (breadcrumb) => () => {
  const timestamp = Date.now() / 1000;
  Sentry.addBreadcrumb(Object.assign({ timestamp }, breadcrumb));
};
