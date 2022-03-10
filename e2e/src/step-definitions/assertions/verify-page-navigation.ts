import { Then } from '@cucumber/cucumber';
import { ElementKey } from '../../env/global';
import { getElementLocator } from '../../support/web-element-helper';
import { ScenarioWorld } from '../setup/world'
import { waitFor } from '../../support/wait-for-behavior';

Then(
  /^I should be on the "([^"]*)" page$/,
  async function (string) {
  // Write code here that turns the phrase above into concrete actions
  return 'pending';
});