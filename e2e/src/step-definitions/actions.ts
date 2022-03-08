import { When } from '@cucumber/cucumber';
import { queries } from 'playwright-testing-library';
import { getElementLocator } from '../support/web-element-helper';

When(
  /^I click "([^"]*)" button$/,
  async function(elementKey: string) {
    const {
      screen: { document, page },
      globalVariables,
      globalConfig,
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalVariables, globalConfig);
    const locator = page.locator(elementIdentifier);


    // const locator = await queries.getByRole(document, 'button', { name: elementKey })
    await locator.click();
  }
)