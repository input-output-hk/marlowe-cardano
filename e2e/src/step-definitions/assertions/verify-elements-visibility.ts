import { Then } from '@cucumber/cucumber';
import { expect } from '@playwright/test';
import { getElementLocator } from '../../support/web-element-helper';

Then(
  /^I should see "([^"]*)" text$/,
  async function(elementKey: string) {

    const {
      screen: { page },
      globalVariables,
      globalConfig,
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalVariables, globalConfig);
    const locator = page.locator(elementIdentifier);

    await expect(locator).toBeVisible();
  }
);

Then(
  /^I should see a button with "([^"]*)" text$/,
  async function(elementKey: string) {
    const {
      screen: { page },
      globalVariables,
      globalConfig,
    } = this;

    const elementIdentifier = getElementLocator(page, elementKey, globalVariables, globalConfig);
    const locator = page.locator(elementIdentifier);

    await expect(locator).toBeVisible();
  }
);

