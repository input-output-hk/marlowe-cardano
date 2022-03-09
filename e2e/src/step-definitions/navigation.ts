import { Given } from '@cucumber/cucumber';
import { PageId } from '../env/global';
import {
  navigateToPage,
  currentPathMatchesPageId,
} from '../support/navigation-behavior';
import { ScenarioWorld } from './setup/world';
import { waitFor } from '../support/wait-for-behavior'

Given(
  /^I am on the "([^"]*)" page$/,
  async function(this: ScenarioWorld, pageId: PageId) {
    // Anything we pull off from `this` variable is defined in cucumber world
    const {
      screen: { page },
      globalConfig,
    } = this;

    console.log(`I am on the ${pageId} page`);

    await navigateToPage(page, pageId, globalConfig);

    await waitFor(() => currentPathMatchesPageId(page, pageId, globalConfig))
  }
);