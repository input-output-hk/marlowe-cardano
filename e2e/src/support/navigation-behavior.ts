import { Page } from 'playwright';
import { GlobalConfig, PageId } from '../env/global';

export const navigateToPage = async (
  page: Page,
  pageId: PageId,
  { pagesConfig, hostsConfig }: GlobalConfig
): Promise<void> => {
  const {
    UI_AUTOMATION_HOST: hostName = 'current-sprint',
  } = process.env

  const hostPath = hostsConfig[`${hostName}`];
  const url = new URL(hostPath);
  const pagesConfigItem = pagesConfig[pageId];
  url.pathname = pagesConfigItem.route;

  await page.goto(url.href);
}

const pathMatchesPageId = (
  path: string,
  pageId: PageId,
  { pagesConfig }: GlobalConfig
): boolean => {
  const pageRegexString = pagesConfig[pageId].regex;
  const pageRegex = new RegExp(pageRegexString);
  return pageRegex.test(path);
}

export const currentPathMatchesPageId = (
  page: Page,
  pageId: PageId,
  globalConfig: GlobalConfig,
): boolean => {
  const { pathname, hash } = new URL(page.url());
  const url = new URL(page.url());
  const currentPath = `${pathname}${hash}`;
  console.log("pathname ", pathname)
  console.log("hash ", hash)
  console.log("currentPath ", currentPath)
  return pathMatchesPageId(currentPath, pageId, globalConfig);
}

export const getCurrentPageId = (
  page: Page,
  globalConfig: GlobalConfig,
): PageId => {

  const { pagesConfig } = globalConfig;
  const pageConfigPageIds = Object.keys(pagesConfig);
  const { pathname, hash } = new URL(page.url());
  const currentPath = `${pathname}${hash}`;
  const currentPageId = pageConfigPageIds.find(pageId =>
    pathMatchesPageId(currentPath, pageId, globalConfig)
  );

  if (!currentPageId) {
    throw Error(
      `Failed to get page name from current route ${currentPath}, \
      possible pages: ${JSON.stringify((pagesConfig))}`
    )
  }

  return currentPageId;
}