import { Then } from '@cucumber/cucumber';
import { expect } from '@playwright/test';
import { ElementKey } from '../../env/global'
import { getElementLocator } from '../../support/web-element-helper';

Then(
  /^I should see "([^"]*)" text$/,
  async function(elementKey: string) {

    const {
      screen: { page },
      globalVariables,
      globalConfig,
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalVariables, globalConfig)

    await expect(elementIdentifier).toBeVisible();
  }
);

Then(
  /^I should see a button with "([^"]*)" text$/,
  async function(text: string) {
    const {
      screen: { page },
      globalVariables,
      globalConfig,
    } = this;

    const locator = await page.locator(`button >> text=${text}`);

    await expect(locator).toBeVisible();
  }
);

