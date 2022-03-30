import { Page } from 'playwright';
import { GlobalConfig, PageId } from '../env/global';

export const navigateToPage = async (
  page: Page,
  pageId: PageId,
  { pagesConfig, hostsConfig, applicationId }: GlobalConfig
): Promise<void> => {
  const {
    UI_AUTOMATION_HOST: environmentId = 'current-sprint',
  } = process.env

  const hostsByApp = hostsConfig[`${environmentId}`];
  const hostPath = hostsByApp[`${applicationId}`];
  const url = new URL(hostPath);
  const applicationPagesConfigItem = pagesConfig[applicationId];
  const pagesConfigItem = applicationPagesConfigItem[pageId];
  url.pathname = pagesConfigItem.route;

  await page.goto(url.href);
}

const pathMatchesPageId = (
  pathname: string,
  hash: string,
  pageId: PageId,
  { pagesConfig, applicationId }: GlobalConfig
): boolean => {
  const currentPath = `${pathname}${hash}`;
  const applicationPagesConfig = pagesConfig[applicationId]
  const pageRegexString = applicationPagesConfig[pageId].regex;
  const pageRegex = new RegExp(pageRegexString);
  return pageRegex.test(currentPath);
}

export const currentPathMatchesPageId = (
  page: Page,
  pageId: PageId,
  globalConfig: GlobalConfig,
): boolean => {
  const { pathname, hash } = new URL(page.url());
  const url = new URL(page.url());
  return pathMatchesPageId(pathname, hash, pageId, globalConfig);
}

export const getCurrentPageId = (
  page: Page,
  globalConfig: GlobalConfig,
): PageId => {
  const { pagesConfig, applicationId } = globalConfig;
  const applicationPagesConfig = pagesConfig[applicationId];
  const pageConfigPageIds = Object.keys(applicationPagesConfig);
  const { pathname, hash } = new URL(page.url());
  const currentPageId = pageConfigPageIds.find(pageId =>
    pathMatchesPageId(pathname, hash, pageId, globalConfig)
  );

  if (!currentPageId) {
    throw Error(
      `Failed to get page name from current route ${pathname}/${hash}, \
      possible pages: ${JSON.stringify((pagesConfig))}`
    )
  }

  return currentPageId;
}