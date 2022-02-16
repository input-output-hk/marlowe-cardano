import { Given } from '@cucumber/cucumber';

Given(
  /^I am on the "([^"]*)" page$/,
  async function(pageId: string) {
    const {
      screen: { page },
    } = this;

    console.log(`I am on the ${pageId} page`);

    await page.goto("https://marlowe-playground-currentsprintmarlowe.plutus.aws.iohkdev.io/");
  }
);