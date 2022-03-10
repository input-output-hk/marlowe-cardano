import { Page } from 'playwright';
import { ElementLocator } from '../env/global';

export const clickElement = async(
  page: Page,
  elementIdentifier: ElementLocator,
): Promise<void> => {
  await page.click(elementIdentifier);
}