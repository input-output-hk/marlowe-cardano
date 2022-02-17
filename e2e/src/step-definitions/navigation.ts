import { Given } from '@cucumber/cucumber';
import { PageId } from '../env/global';
import {
  navigateToPage
} from '../support/navigation-behavior';

Given(
  /^I am on the "([^"]*)" page$/,
  async function(pageId: PageId) {
    // Anything we pull off from `this` variable is defined in cucumber world
    const {
      screen: { page },
      globalConfig,
    } = this;

    console.log(`I am on the ${pageId} page`);

    await navigateToPage(page, pageId, globalConfig);
  }
);