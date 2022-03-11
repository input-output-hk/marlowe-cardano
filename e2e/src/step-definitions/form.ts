import { When } from "@cucumber/cucumber";
import { waitFor } from "../support/wait-for-behavior";
import { ScenarioWorld } from './setup/world';
import { getElementLocator } from '../support/web-element-helper';
import { ElementKey } from '../env/global';
import { inputValue } from '../support/html-behavior';

When(
  /^I fill in the "([^"]*)" input with "([^"]*)"$/,
  async function(this: ScenarioWorld, elementKey: ElementKey, input: string) {
    const {
      screen: { page },
      globalConfig,
    } = this;

    console.log(`I fill in the ${elementKey} input with ${input}`);

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);

    await waitFor(async() => {
      const result = await page.waitForSelector(elementIdentifier, {
        state: 'visible'
      })

      if (result) {
        await inputValue(page, elementIdentifier, input)

        return result;
      }
    })
  }
)