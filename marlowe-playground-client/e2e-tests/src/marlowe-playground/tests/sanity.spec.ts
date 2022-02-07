import { test, expect } from '@playwright/test';
import envs from '../envs.json';

const { HomePage } = require('../../pageobject/home');

envs.forEach(function(env) {
  const baseUrl = env.url;

  test(`Visit marlowe playground home page at ${baseUrl}`, async ({ page }) => {
    // given
    const homePage = new HomePage(page, baseUrl);
    await homePage.goto(baseUrl);

  })
});