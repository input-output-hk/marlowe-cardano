import { ElementHandle } from 'playwright-testing-library/dist/typedefs';

export const clickElement = async(
  locator: ElementHandle,
): Promise<void> => {
  await locator.click()
}

export const inputValue = async (
  locator: ElementHandle,
  input: string,
): Promise<void> => {
  await locator.focus();
  await locator.fill(input);
}

export const selectValue = async(
  locator: ElementHandle,
  option: string,
): Promise<void> => {
  await locator.focus();
  await locator.selectOption(option);
}