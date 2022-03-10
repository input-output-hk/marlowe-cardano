import { When } from '@cucumber/cucumber';
import { ScenarioWorld } from './setup/world';
// import { queries } from 'playwright-testing-library';
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
      // screen: { document, page },
      screen: { page },
      globalConfig,
    } = this;

    console.log(`I click the ${elementKey} (?:button|link|icon|element)`);

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);

    await waitFor(async() => {
      const result = await page.waitForSelector(elementIdentifier, {
        state: 'visible',
      })

      if (result) {
        await clickElement(page, elementIdentifier);
      }

      console.log("PAGE URL ", page.url())
      return result;
      // const locator = await queries.getByRole(document, 'button', { name: elementKey })
    })
  }
)