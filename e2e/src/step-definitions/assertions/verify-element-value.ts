import { Then } from '@cucumber/cucumber';
import { ElementKey } from '../../env/global';
import { queries, getDocument } from 'playwright-testing-library';
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
    const document = await getDocument(page);

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;
    await waitFor(async() => {
      const locator = await queries.getByRole(document, role, { name })
      const isElementVisible = await locator.isVisible();
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
    const document = await getDocument(page);
    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;
    await waitFor(async() => {
      const locator = await queries.getByRole(document, role, { name })
      const elementText = await locator.textContent();
      console.log("ExepctedElement: ", expectedElementText);
      console.log("ElementText: ", elementText);
      return elementText?.includes(expectedElementText);
    })
  }
);
