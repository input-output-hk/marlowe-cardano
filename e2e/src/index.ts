// The purpose of this file is to be able to enable dynamic paramters to be passed via the actual cucumber arguments 
// that we pass when calling the cucumber executable.

import dotenv from 'dotenv';
import { env, getJsonFromFile } from './env/parseEnv';
import {
  GlobalConfig,
  HostsConfig,
  PagesConfig,
} from './env/global';

dotenv.config({path: env('COMMON_CONFIG_FILE')});

const hostsConfig: HostsConfig = getJsonFromFile(env('HOSTS_URLS_PATH'))

console.log("hostsConfig ", hostsConfig);

const pagesConfig: PagesConfig = getJsonFromFile(env('PAGE_URLS_PATH'))

console.log("pagesConfig ", pagesConfig);

const worldParameters: GlobalConfig = {
  hostsConfig,
  pagesConfig,
}

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