import { expect, Locator, Page } from '@playwright/test';

export class HomePage {
  readonly pageTitle :string = 'Get started';
  readonly page: Page;
  readonly javascriptEnvLink: Locator;
  readonly haskellEnvLink: Locator;
  readonly marloweEnvLink: Locator;
  readonly blocklyEnvLink: Locator;
  readonly baseUrl: string;


  constructor(page: Page, baseUrl: string) {
    this.page = page
    this.javascriptEnvLink = page.locator("section >> text=Start in Javascript");
    this.haskellEnvLink = page.locator("section >> text=Start in Haskell");
    this.marloweEnvLink = page.locator("section >> text=Start in Marlowe");
    this.blocklyEnvLink = page.locator("section >> text=Start in Blockly");
  }

  async goto(base: string) {
    await this.page.goto(base);
    await expect(this.javascriptEnvLink).toBeVisible();
  }
}