import { Given } from '@cucumber/cucumber';

Given(
  /^I am on the "([^"]*)" page$/,
  async function(pageId: string) {
    console.log(`I am on the ${pageId} page`);

    await global.page.goto("https://marlowe-playground-currentsprintmarlowe.plutus.aws.iohkdev.io/");
  }
);