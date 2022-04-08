import { Then } from '@cucumber/cucumber';
import { ElementKey, FixtureKey } from '../../env/global';
import { queries, getDocument } from 'playwright-testing-library';
import { getElementLocator } from '../../support/web-element-helper';
import { getFixtureText } from '../../support/fixture-helper';
import { ScenarioWorld } from '../setup/world'
import { waitFor } from '../../support/wait-for-behavior';

Then(
  /^I should see a button with "([^"]*)" text$/,
  async function(this: ScenarioWorld, elementKey: ElementKey) {
    const {
      screen: { page },
      globalConfig
    } = this;
    const document = await getDocument(page);

    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;
    await waitFor(async() => {
      try {
        const locator = await queries.getByRole(document, role, { name })
        const isElementVisible = await locator.isVisible();
        return isElementVisible;
      } catch {
        return false;
      }
    })
  }
);

Then(
  /^the "([^"]*)" should contain "([^"]*)" text$/,
  async function(this: ScenarioWorld, elementKey: ElementKey, expectedElementText: string) {
    const {
      screen: { page },
      globalConfig
    } = this;
    const document = await getDocument(page);
    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;
    await waitFor(async() => {
      const locator = await queries.getByRole(document, role, { name })
      const elementText = await locator.textContent();
      return elementText?.includes(expectedElementText);
    })
  }
);

Then(
  /^the "playground editor" should contain "([^"]*)" contract code$/,
  async function(this: ScenarioWorld, contractFixtureKey: FixtureKey) {
    const {
      screen: { page },
      globalConfig
    } = this;
    const document = await getDocument(page);
    const elementIdentifier = getElementLocator(page, "playground editor", globalConfig);
    const { role, name } = elementIdentifier;
    const fixture = getFixtureText(contractFixtureKey, globalConfig);
    await waitFor(async() => {

      const locator = await page.locator('.monaco-scrollable-element');
      const elementText = await locator.textContent()
      // const locator = await queries.getByRole(document, role, { name })
      // console.log("LOCATOR: ", locator)
      // const elementText = await locator.innerText()
      // console.log("Text Length: ", elementText.length)
      console.log("VALUE: ", elementText)

      return elementText?.includes(fixture);
    })
  }
);

Then(
  /^I should see the "([^"]*)"$/,
  async function(this: ScenarioWorld, elementKey: ElementKey) {
    const {
      screen: { page },
      globalConfig
    } = this;

    const document = await getDocument(page);
    const elementIdentifier = getElementLocator(page, elementKey, globalConfig);
    const { role, name } = elementIdentifier;
    await waitFor(async() => {
      try {
        const locator = await queries.getByRole(document, role, { name })
        const isElementVisible = await locator.isVisible();
        return isElementVisible;
      } catch {
        return false;
      }
    })
  }
)