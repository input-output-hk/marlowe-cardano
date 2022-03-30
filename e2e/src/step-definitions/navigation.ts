import { Given } from '@cucumber/cucumber';
import { PageId, ApplicationId } from '../env/global';
import {
  navigateToPage,
  currentPathMatchesPageId,
} from '../support/navigation-behavior';
import { ScenarioWorld } from './setup/world';
import { waitFor } from '../support/wait-for-behavior'

Given(
  /^I am on the "([^"]*)" page of the "([^"]*)" application$/,
  async function(this: ScenarioWorld, pageId: PageId, applicationId: ApplicationId) {
    // Anything we pull off from `this` variable is defined in cucumber world
    const {
      screen: { page },
      globalConfig,
    } = this;
    this.globalConfig.applicationId = applicationId;
    console.log(`I am on the ${pageId} page of the ${applicationId} application`);

    await navigateToPage(page, pageId, globalConfig);

    await waitFor(() => currentPathMatchesPageId(page, pageId, globalConfig))
  }
);

Given(
  /^I am directed to the "([^"]*)" page$/,
  async function(this: ScenarioWorld, pageId: PageId) {
    const {
      screen: { page },
      globalConfig
    } = this;
    console.log(`I am directed to the ${pageId} page`);

    await waitFor(() => currentPathMatchesPageId(page, pageId, globalConfig))
  }
)