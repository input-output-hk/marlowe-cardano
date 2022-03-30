import { Page } from 'playwright';
import { ApplicationId, ElementKey, ElementLocator, GlobalConfig } from '../env/global';
import { getCurrentPageId } from './navigation-behavior';

export const getElementLocator = (
  page: Page,
  elementKey: ElementKey,
  applicationId: ApplicationId,
  globalConfig: GlobalConfig
): ElementLocator => {

  const currentPage = getCurrentPageId(page, applicationId, globalConfig);
  const { pageElementMappings } = globalConfig;

  return pageElementMappings[currentPage]?.[elementKey] || pageElementMappings.common?.[elementKey]
}