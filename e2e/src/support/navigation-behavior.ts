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

  console.log("hostPath ", hostPath);

  const url = new URL(hostPath);

  console.log("url ", url)

  const pagesConfigItem = pagesConfig[pageId];
  url.pathname = pagesConfigItem.route;

  console.log("pages route ", url.pathname)

  await page.goto(url.href);
}
