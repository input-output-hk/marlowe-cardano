import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from './setup/world';
import { queries, getDocument } from 'playwright-testing-library';
import {
  clickElement
} from '../support/html-behavior';
import { waitFor } from '../support/wait-for-behavior';
import { getElementLocator } from '../support/web-element-helper';
import { ElementKey } from '../env/global';

When(
  /^I click the "([^"]*)" (?:button|link|icon|element)$/,
  async function(this: ScenarioWorld, elementKey: ElementKey) {
    const {
      screen: { page },
      globalConfig
    } = this;

    console.log(`I click the ${elementKey} (?:button|link|icon|element|text)`);
    const document = await getDocument(page);

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;
    const locator = await queries.getByRole(document, role, { name })

    await waitFor(async() => {
      const result = await locator.isVisible();
      if (result) {
        await clickElement(locator);
      }

      return result;
    })
  }
)