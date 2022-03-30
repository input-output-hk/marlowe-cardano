import { Then } from '@cucumber/cucumber';
import { ElementKey } from '../../env/global';
import { getElementLocator } from '../../support/web-element-helper';
import { queries } from 'playwright-testing-library';
import { ScenarioWorld } from '../setup/world'
import { waitFor } from '../../support/wait-for-behavior';

Then(
  /^I should see "([^"]*)" text$/,
  async function(this: ScenarioWorld, elementKey: ElementKey) {

    const {
      screen: { page, document },
      globalConfig
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;

    await waitFor(async() => {
      const locator = await queries.getByRole(document, role, { name })
      const isElementVisible = await locator.isVisible()
      return isElementVisible;
    })
  }
);
