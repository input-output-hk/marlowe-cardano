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
