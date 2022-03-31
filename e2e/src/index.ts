// The purpose of this file is to be able to enable dynamic paramters to be passed via the actual cucumber arguments 
// that we pass when calling the cucumber executable.

import dotenv from 'dotenv';
import { env, getJsonFromFile } from './env/parseEnv';
import {
  GlobalConfig,
  HostsConfig,
  PagesConfig,
  PageElementMappings,
  FixtureMappings,
  ApplicationId,
  ApplicationPageElementMappings
} from './env/global';
import * as fs from 'fs';

dotenv.config({path: env('COMMON_CONFIG_FILE')})
const hostsConfig: HostsConfig = getJsonFromFile(env('HOSTS_URLS_PATH'))
const pagesConfig: PagesConfig = getJsonFromFile(env('PAGE_URLS_PATH'))
const playgroundPageMappingFiles = fs.readdirSync(`${process.cwd()}${env('PLAYGROUND_PAGE_ELEMENTS_PATH')}`)
const runPageMappingFiles = fs.readdirSync(`${process.cwd()}${env('RUN_PAGE_ELEMENTS_PATH')}`)
const applicationId: ApplicationId = '';

const playgroundPageElementMappings: PageElementMappings = playgroundPageMappingFiles.reduce(
  (pageElementConfigAcc, file) => {
    const key = file.replace('.json', '');
    const elementMappings = getJsonFromFile(`${env('PLAYGROUND_PAGE_ELEMENTS_PATH')}${file}`);
    return { ...pageElementConfigAcc, [key]: elementMappings}
  },
  {}
);

const runPageElementMappings: PageElementMappings = runPageMappingFiles.reduce(
  (pageElementConfigAcc, file) => {
    const key = file.replace('.json', '');
    const elementMappings = getJsonFromFile(`${env('RUN_PAGE_ELEMENTS_PATH')}${file}`);
    return { ...pageElementConfigAcc, [key]: elementMappings}
  },
  {}
);

const fixtureMappingFiles = fs.readdirSync(`${process.cwd()}${env('FIXTURES_PATH')}`)
const fixtureMappings: FixtureMappings = fixtureMappingFiles.reduce(
  (fixtureConfigAcc, file) => {
    const key = file.replace('.json', '');
    const fixturesMappings = getJsonFromFile(`${env('FIXTURES_PATH')}${file}`);
    return { ...fixtureConfigAcc, [key]: fixturesMappings}
  },
  {}
);

const applicationPageElementMappings: ApplicationPageElementMappings = {
  "marlowe playground": playgroundPageElementMappings,
  "marlowe run": runPageElementMappings
}

const worldParameters: GlobalConfig = {
  hostsConfig,
  pagesConfig,
  applicationPageElementMappings,
  fixtureMappings,
  applicationId,
};

const common = `./src/features/**/*.feature \
                --require-module ts-node/register \
                --require ./src/step-definitions/**/**/*.ts \
                --world-parameters ${JSON.stringify(worldParameters)} \
                -f json:./reports/reports.json \
                --format progress-bar`;

const dev = `${common} --tags '@dev'`;
const smoke = `${common} --tags '@smoke'`;
const regression = `${common} --tags '@regression'`;

export { dev, smoke, regression }