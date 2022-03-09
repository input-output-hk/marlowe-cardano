import { When } from '@cucumber/cucumber';
import { queries } from 'playwright-testing-library';
import { getElementLocator } from '../support/web-element-helper';
import { ElementKey } from '../env/global';
import { ScenarioWorld } from './setup/world';

When(
  /^I click "([^"]*)" button$/,
  async function(this: ScenarioWorld, elementKey: ElementKey) {
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