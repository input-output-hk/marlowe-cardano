import { Then } from '@cucumber/cucumber';
import { ElementKey } from '../../env/global';
import { getElementLocator } from '../../support/web-element-helper';
import { ScenarioWorld } from '../setup/world'
import { waitFor } from '../../support/wait-for-behavior';

Then(
  /^I should see a button with "([^"]*)" text$/,
  async function(this: ScenarioWorld, elementKey: ElementKey) {
    const {
      screen: { page },
      globalConfig,
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    await waitFor(async() => {
      const isElementVisible = (await page.$(elementIdentifier)) != null
      return isElementVisible;
    })
  }
);

Then(
  /^the "([^"]*)" should contain "([^"]*)" text$/,
  async function(this: ScenarioWorld, elementKey: ElementKey, expectedElementText: string) {
    const {
      screen: { page },
      globalConfig,
    } = this;
    console.log("elementKey ", elementKey)
    console.log("expectedValue ", expectedElementText)
    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    console.log("ELEMENT IDENTIFIER ", elementIdentifier);
    await waitFor(async() => {
      const elementText = await page.textContent(elementIdentifier);
      return elementText?.includes(expectedElementText);
    })
  }
);
