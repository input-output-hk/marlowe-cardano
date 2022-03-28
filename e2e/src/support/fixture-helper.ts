import { FixtureKey, GlobalConfig } from '../env/global';

export const getFixtureText = (
  fixtureKey: FixtureKey,
  globalConfig: GlobalConfig
): string => {

  const { fixtureMappings } = globalConfig;
  return fixtureMappings[fixtureKey]?.[fixtureKey];
}