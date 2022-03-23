import { When } from "@cucumber/cucumber";
import { waitFor } from "../support/wait-for-behavior";
import { queries, getDocument } from 'playwright-testing-library';
import { ScenarioWorld } from './setup/world';
import { getElementLocator } from '../support/web-element-helper';
import { ElementKey } from '../env/global';
import {
  inputValue,
  selectValue
} from '../support/html-behavior';

When(
  /^I fill in the "([^"]*)" input with "([^"]*)"$/,
  async function(this: ScenarioWorld, elementKey: ElementKey, input: string) {
    const {
      screen: { page },
      globalConfig,
    } = this;


    console.log(`I fill in the ${elementKey} input with ${input}`);

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;
    const document = await getDocument(page);

    await waitFor(async() => {
      const locator = await queries.getByRole(document, role, { name })
      const result = await locator.isVisible();

      if (result) {
        await inputValue(locator, input)
        return result;
      }
    })
  }
)

When(
  /^I select the "([^"]*)" option from the "([^"]*)"$/,
  async function(this: ScenarioWorld, option: string, elementKey: ElementKey) {
    const {
      screen: { page },
      globalConfig,
    } = this;

    console.log(`I select the ${option} option from the ${elementKey}`)
    const document = await getDocument(page);

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;

    await waitFor(async() => {
      const locator = await queries.getByRole(document, role, { name })
      const result = await locator.isVisible();

      if (result) {
        await selectValue(locator, option)
        return result;
      }
    })
  }
)