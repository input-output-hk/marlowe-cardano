import { Page } from 'playwright';
import { ElementKey, ElementLocator, GlobalConfig } from '../env/global';
import { getCurrentPageId } from './navigation-behavior';

export const getElementLocator = (
  page: Page,
  elementKey: ElementKey,
  globalConfig: GlobalConfig
): ElementLocator => {

  const currentPage = getCurrentPageId(page, globalConfig);

  console.log("PAGE ELEMENT CURRENT PAGE", currentPage)
  const { pageElementMappings } = globalConfig;

  console.log("PAGE ELEMENT MAPPINGS ", pageElementMappings)

  return pageElementMappings[currentPage]?.[elementKey] || pageElementMappings.common?.[elementKey]
}