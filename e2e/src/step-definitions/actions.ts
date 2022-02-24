import { When } from '@cucumber/cucumber';

When(
  /^I click "([^"]*)" button$/,
  async function(buttonText: string) {
    const {
      screen: { page },
    } = this;

    const locator = await page.locator(`button >> text=${buttonText}`);
    await locator.click();
  }
)